{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction (
  TransactionWitness (..),
  validateTransaction,
  outputHasValueSanity,
) where

import Control.Lens ((.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Function (($), (&))
import Data.OpenApi (OpenApiItems (..), OpenApiType (..), Referenced (Inline), ToSchema (..), declareSchemaRef, type_)
import Data.OpenApi.Internal.Schema (named)
import Data.OpenApi.Lens (items, properties, required)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic, Generic1, (:*:) (..), (:.:) (..))
import GHC.IsList (IsList (..))
import GHC.TypeNats (KnownNat, type (-))
import ZkFold.Algebra.Class (
  AdditiveSemigroup (..),
  FromConstant (fromConstant),
  MultiplicativeMonoid (..),
  MultiplicativeSemigroup (..),
  Zero (..),
 )
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq
import ZkFold.Data.HFunctor.Classes (HShow)
import ZkFold.Data.Ord ((>), (>=))
import ZkFold.Data.Vector (Vector, Zip (..))
import ZkFold.Data.Vector qualified as Vector
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Algorithm.EdDSA (eddsaVerify)
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (hash)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Int (Int (..))
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry)
import ZkFold.Symbolic.Data.MerkleTree qualified as MerkleTree
import ZkFold.Symbolic.Data.UInt (toNative)
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Types.Orphans ()
import ZkFold.Symbolic.Ledger.Utils (unsafeToVector')

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications -XDataKinds
-- >>> import qualified Data.Aeson                 as Aeson
-- >>> import           Data.Proxy
-- >>> import qualified Data.ByteString.Lazy.Char8 as BSL
-- >>> import Data.OpenApi.Internal.Schema
-- >>> import Data.OpenApi.Internal.Utils (encodePretty)
-- >>> import ZkFold.Symbolic.Ledger.Types.Field

-- | Transaction witness for validating transaction.
--
-- The signature set ('twSignatures') contains one entry per unique signer,
-- following the Cardano-style witness model. This is more efficient than
-- providing a signature per input, as it avoids redundant EdDSA verifications
-- when multiple inputs belong to the same signer.
data TransactionWitness ud s n a context = TransactionWitness
  { twSignatures :: (Vector s :.: (PublicKey :*: EdDSAPoint :*: EdDSAScalarField)) context
  , twInputs :: (Vector n :.: (MerkleEntry ud :*: UTxO a)) context
  , twOutputs :: (Vector n :.: MerkleEntry ud) context
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

deriving stock instance HShow context => Haskell.Show (TransactionWitness ud s n a context)

instance ToJSON (TransactionWitness ud s n a RollupBFInterpreter) where
  toJSON (TransactionWitness sigs ins outs) =
    let sigsVec = unComp1 sigs
        sigsList = Vector.fromVector sigsVec
        encodeSig (pk :*: rPoint :*: s) =
          object
            [ "publicKey" .= pk
            , "r" .= rPoint
            , "s" .= s
            ]
        insVec = unComp1 ins
        insList = Vector.fromVector insVec
        encodeIn (me :*: utxo) =
          object
            [ "merkleEntry" .= me
            , "utxo" .= utxo
            ]
        outsVec = unComp1 outs
        outsList = Vector.fromVector outsVec
     in object
          [ "signatures" .= Haskell.fmap encodeSig sigsList
          , "inputs" .= Haskell.fmap encodeIn insList
          , "outputs" .= outsList
          ]

instance (KnownNat s, KnownNat n) => FromJSON (TransactionWitness ud s n a RollupBFInterpreter) where
  parseJSON =
    withObject
      "TransactionWitness"
      ( \o -> do
          sigsVals <- o .: "signatures"
          sigsList <-
            Haskell.traverse
              ( withObject
                  "Signature"
                  ( \so -> do
                      pk <- so .: "publicKey"
                      rPoint <- so .: "r"
                      s <- so .: "s"
                      Haskell.pure (pk :*: rPoint :*: s)
                  )
              )
              sigsVals
          insVals <- o .: "inputs"
          insList <-
            Haskell.traverse
              ( withObject
                  "Input"
                  ( \io -> do
                      me <- io .: "merkleEntry"
                      utxo <- io .: "utxo"
                      Haskell.pure (me :*: utxo)
                  )
              )
              insVals
          outsList <- o .: "outputs"
          let twSignatures = Comp1 (unsafeToVector' sigsList)
              twInputs = Comp1 (unsafeToVector' insList)
              twOutputs = Comp1 (unsafeToVector' outsList)
          Haskell.pure (TransactionWitness twSignatures twInputs twOutputs)
      )

instance
  forall ud s n a
   . (KnownNat ud, KnownNat s, KnownNat n, KnownNat a, KnownNat (ud - 1))
  => ToSchema (TransactionWitness ud s n a RollupBFInterpreter)
  where
  declareNamedSchema _ = do
    meRef <- declareSchemaRef (Proxy @(MerkleEntry ud RollupBFInterpreter))
    utxoRef <- declareSchemaRef (Proxy @(UTxO a RollupBFInterpreter))
    rRef <- declareSchemaRef (Proxy @(EdDSAPoint RollupBFInterpreter))
    sRef <- declareSchemaRef (Proxy @(EdDSAScalarField RollupBFInterpreter))
    pkRef <- declareSchemaRef (Proxy @(PublicKey RollupBFInterpreter))
    outsRef <- declareSchemaRef (Proxy @((:.:) (Vector n) (MerkleEntry ud) RollupBFInterpreter))

    let sigSchema =
          Haskell.mempty
            & type_ ?~ OpenApiObject
            & properties
              .~ fromList
                [ ("publicKey", pkRef)
                , ("r", rRef)
                , ("s", sRef)
                ]
            & required .~ ["publicKey", "r", "s"]

        sigsSchema =
          Haskell.mempty
            & type_ ?~ OpenApiArray
            & items ?~ OpenApiItemsObject (Inline sigSchema)

        inputSchema =
          Haskell.mempty
            & type_ ?~ OpenApiObject
            & properties
              .~ fromList
                [ ("merkleEntry", meRef)
                , ("utxo", utxoRef)
                ]
            & required .~ ["merkleEntry", "utxo"]

        inputsSchema =
          Haskell.mempty
            & type_ ?~ OpenApiArray
            & items ?~ OpenApiItemsObject (Inline inputSchema)

        schema =
          Haskell.mempty
            & type_ ?~ OpenApiObject
            & properties
              .~ fromList
                [ ("signatures", Inline sigsSchema)
                , ("inputs", Inline inputsSchema)
                , ("outputs", outsRef)
                ]
            & required .~ ["signatures", "inputs", "outputs"]

    Haskell.pure (named "TransactionWitness" schema)

-- | Validate transaction. See note [State validation] for details.
validateTransaction
  :: forall ud s bo n a context
   . SignatureTransaction ud s n a context
  => FieldElement context
  -- ^ UTxO tree root hash.
  -> (Vector bo :.: Output a) context
  -- ^ Bridged out outputs.
  -> Transaction n a context
  -- ^ Transaction.
  -> TransactionWitness ud s n a context
  -- ^ Transaction witness.
  -> ( FieldElement
         :*: Bool
         :*: FieldElement
         :*: (Vector n :.: FieldElement)
         :*: (Vector n :.: (Bool :*: FieldElement :*: FieldElement))
     )
       context
  -- ^ Result of validation:
  -- (1) number of bridged out outputs,
  -- (2) validity,
  -- (3) updated UTxO tree root hash,
  -- (4) input delta: packed leaf positions of consumed inputs (new value is always nullUTxOHash),
  -- (5) output delta per output: (isActive, packed leaf position, new leaf hash).
  --     isActive is false for bridge-out and null outputs (no tree change).
validateTransaction utxoRoot bridgedOutOutputs tx txw =
  let
    txId' = txId tx & Base.hHash

    -- Verify signatures and compute signed addresses.
    -- Each signature slot verifies one EdDSA signature and computes the signer's address.
    -- This is cheaper than per-input verification when s < n (common case: single signer).
    sigAddresses :: Vector s (FieldElement context)
    sigAddresses = Haskell.fmap (\(pk :*: _ :*: _) -> hashFn pk) (unComp1 txw.twSignatures)
    areSigsValid =
      foldl'
        ( \acc (pk :*: rPoint :*: s) ->
            acc && eddsaVerify hashFn pk txId' (rPoint :*: s)
        )
        (true :: Bool context)
        (unComp1 txw.twSignatures)

    inputAssets = unComp1 txw.twInputs & Haskell.fmap (\(_ :*: utxo) -> unComp1 utxo.uOutput.oAssets)
    outputsAssets = unComp1 tx.outputs & Haskell.fmap (\(output :*: _) -> unComp1 output.oAssets)

    -- Asset-balanced check using random linear combination (Schwartz-Zippel).
    -- For each asset (policy, name, qty), compute qty * ((policy + 1) * r + (name + 1))
    -- and sum across all inputs/outputs. If sums match, the transaction is balanced
    -- with overwhelming probability (soundness error ≤ 1/|F| ≈ 2^{-255}).
    -- The +1 offsets ensure ADA (policy=0, name=0) gets weight r+1 ≠ 0.
    -- r = txId' is bound to the transaction content, so the prover cannot
    -- independently choose r to forge a false balance.
    r = txId'
    weightedSum =
      foldl'
        ( foldl'
            ( \s av ->
                let qtyFe = toNative (uint av.assetQuantity)
                 in s + qtyFe * ((av.assetPolicy + one) * r + (av.assetName + one))
            )
        )
    sIn = weightedSum (zero :: FieldElement context) inputAssets
    sOut = weightedSum (zero :: FieldElement context) outputsAssets
    isBalanced = sIn == sOut
    inputsWithWitness = zipWith (:*:) (unComp1 tx.inputs) (unComp1 txw.twInputs)

    -- Input delta: packed positions extracted from witness (no hash needed — always nullUTxOHash).
    inputDelta :: (Vector n :.: FieldElement) context
    inputDelta =
      Comp1 $
        Haskell.fmap
          (\(me :*: _) -> MerkleTree.packIndex (MerkleTree.position me))
          (unComp1 txw.twInputs)

    (isInsValid :*: allInputsNull :*: updatedRootForInputs) =
      foldl'
        ( \(isInsValidAcc :*: allNullAcc :*: rootAcc) (inputRef :*: (merkleEntry :*: utxo)) ->
            let
              nullUTxOHash' = nullUTxOHash @a @context
              utxoHash :: HashSimple context = hash utxo & Base.hHash
              (isInTree, updatedRoot) =
                MerkleTree.containsAndReplaceRoot
                  merkleEntry
                  nullUTxOHash'
                  rootAcc
              isNullUTxO = utxoHash == nullUTxOHash'
              -- Check that the input's address matches one of the signed addresses.
              addressMatch = foldl' (\found addr -> found || addr == utxo.uOutput.oAddress) false sigAddresses
              isValid' =
                isInsValidAcc
                  && (inputRef == utxo.uRef)
                  && (utxoHash == MerkleTree.value merkleEntry)
                  && isInTree
                  && ifThenElse isNullUTxO true addressMatch
             in
              (isValid' :*: (allNullAcc && isNullUTxO) :*: updatedRoot)
        )
        ((true :: Bool context) :*: (true :: Bool context) :*: utxoRoot)
        inputsWithWitness
    outputsWithWitness = zipWith (:*:) (unComp1 tx.outputs) (unComp1 txw.twOutputs)

    -- Output fold: collect delta entries alongside validation.
    -- utxoHash is computed before ifThenElse to avoid redundancy
    -- (the circuit evaluates both branches regardless).
    ((bouts :*: _ :*: outsValid :*: updatedRootForOutputs), outputDeltasRev) =
      foldl'
        ( \((boutsAcc :*: outputIx :*: outsValidAcc :*: rootAcc), deltasAcc) ((output :*: bout) :*: merkleEntry) ->
            let isNull = output == nullOutput
                sanity = outputHasValueSanity output
                -- Compute UTxO hash unconditionally (reused in tree update below)
                utxo = UTxO {uRef = OutputRef {orTxId = txId', orIndex = outputIx}, uOutput = output}
                utxoHash = hash utxo & Base.hHash
                pos = MerkleTree.packIndex (MerkleTree.position merkleEntry)
                isActive = not bout && not isNull
                deltaEntry = isActive :*: pos :*: utxoHash
                (isInTree, updatedRoot) =
                  MerkleTree.containsAndReplaceRoot
                    merkleEntry
                    utxoHash
                    rootAcc
                circuitResult =
                  ifThenElse
                    bout
                    ( (boutsAcc + one)
                        :*: (outputIx + one)
                        :*: ( outsValidAcc
                                && foldl' (\found boutput -> found || output == boutput) false (unComp1 bridgedOutOutputs)
                                && not isNull
                                && sanity
                            )
                        :*: rootAcc
                    )
                    ( boutsAcc
                        :*: (outputIx + one)
                        :*: ( ( outsValidAcc
                                  && ifThenElse
                                    isNull
                                    true
                                    ( isInTree
                                        && (MerkleTree.value merkleEntry == nullUTxOHash @a @context)
                                        && sanity
                                    )
                              )
                                :*: ifThenElse
                                  isNull
                                  rootAcc
                                  updatedRoot
                            )
                    )
             in (circuitResult, deltaEntry : deltasAcc)
        )
        ((zero :*: zero :*: (true :: Bool context) :*: updatedRootForInputs), [])
        outputsWithWitness
    outputDelta = Comp1 (unsafeToVector' (Haskell.reverse outputDeltasRev))
   in
    ( bouts
        :*: (outsValid && isInsValid && ifThenElse allInputsNull true areSigsValid && isBalanced) -- Note that we don't need to check if transaction consumes at least one input or is null entirely as our transaction currently only has two fields, namely, inputs & outputs and if thus inputs are null, outputs are null too.
        :*: updatedRootForOutputs
        :*: inputDelta
        :*: outputDelta
    )

-- | Check if output has sane value.
--
-- We check if the output has at least one ada and all non-null assets have strictly positive quantity.
outputHasValueSanity
  :: forall a context
   . (KnownRegistersAssetQuantity context, Symbolic context)
  => Output a context
  -> Bool context
outputHasValueSanity output =
  let adaCheck :*: allNonNegCheck =
        foldl'
          ( \(found :*: allNonNegAcc) asset ->
              ( found
                  || ( asset.assetPolicy
                         == adaPolicy
                         && asset.assetName
                         == adaName
                         && asset.assetQuantity
                         >= fromConstant @Haskell.Integer 1_000_000
                     )
              )
                :*: (allNonNegAcc && (asset.assetQuantity > zero || (asset.assetPolicy == zero && asset.assetName == zero)))
          )
          (false :*: true)
          (unComp1 (oAssets output))
   in adaCheck && allNonNegCheck
