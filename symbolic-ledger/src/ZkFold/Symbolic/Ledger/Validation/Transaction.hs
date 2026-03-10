{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction (
  TransactionWitness (..),
  validateTransaction,
  outputHasValueSanity,
) where

import Control.Lens ((.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Function ((&))
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
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry, MerkleTree)
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
data TransactionWitness ud n a context = TransactionWitness
  { twInputs :: (Vector n :.: (MerkleEntry ud :*: UTxO a :*: EdDSAPoint :*: EdDSAScalarField :*: PublicKey)) context
  , twOutputs :: (Vector n :.: MerkleEntry ud) context
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

deriving stock instance HShow context => Haskell.Show (TransactionWitness ud n a context)

instance ToJSON (TransactionWitness ud n a RollupBFInterpreter) where
  toJSON (TransactionWitness ins outs) =
    let insVec = unComp1 ins
        insList = Vector.fromVector insVec
        encodeIn (me :*: utxo :*: rPoint :*: s :*: pk) =
          object
            [ "merkleEntry" .= me
            , "utxo" .= utxo
            , "r" .= rPoint
            , "s" .= s
            , "publicKey" .= pk
            ]
        outsVec = unComp1 outs
        outsList = Vector.fromVector outsVec
     in object
          [ "inputs" .= Haskell.fmap encodeIn insList
          , "outputs" .= outsList
          ]

instance KnownNat n => FromJSON (TransactionWitness ud n a RollupBFInterpreter) where
  parseJSON =
    withObject
      "TransactionWitness"
      ( \o -> do
          insVals <- o .: "inputs"
          insList <-
            Haskell.traverse
              ( withObject
                  "Input"
                  ( \io -> do
                      me <- io .: "merkleEntry"
                      utxo <- io .: "utxo"
                      rPoint <- io .: "r"
                      s <- io .: "s"
                      pk <- io .: "publicKey"
                      Haskell.pure (me :*: utxo :*: rPoint :*: s :*: pk)
                  )
              )
              insVals
          outsList <- o .: "outputs"
          let twInputs = Comp1 (unsafeToVector' insList)
              twOutputs = Comp1 (unsafeToVector' outsList)
          Haskell.pure (TransactionWitness twInputs twOutputs)
      )

-- |
-- >>> BSL.putStrLn $ encodePretty $ toSchema (Proxy :: Proxy (TransactionWitness 2 1 1 RollupBFInterpreter))
-- {
--     "properties": {
--         "inputs": {
--             "items": {
--                 "properties": {
--                     "merkleEntry": {
--                         "$ref": "#/components/schemas/MerkleEntry_2_(Interpreter_*_(Zp_52435875175126190479447740508185965837690552500527637822603658699938581184513))"
--                     },
--                     "publicKey": {
--                         "$ref": "#/components/schemas/AffinePoint"
--                     },
--                     "r": {
--                         "$ref": "#/components/schemas/AffinePoint"
--                     },
--                     "s": {
--                         "type": "integer"
--                     },
--                     "utxo": {
--                         "$ref": "#/components/schemas/UTxO_1_(Interpreter_*_(Zp_52435875175126190479447740508185965837690552500527637822603658699938581184513))"
--                     }
--                 },
--                 "required": [
--                     "merkleEntry",
--                     "utxo",
--                     "r",
--                     "s",
--                     "publicKey"
--                 ],
--                 "type": "object"
--             },
--             "type": "array"
--         },
--         "outputs": {
--             "items": {
--                 "$ref": "#/components/schemas/MerkleEntry_2_(Interpreter_*_(Zp_52435875175126190479447740508185965837690552500527637822603658699938581184513))"
--             },
--             "type": "array"
--         }
--     },
--     "required": [
--         "inputs",
--         "outputs"
--     ],
--     "type": "object"
-- }
instance
  forall ud n a
   . (KnownNat ud, KnownNat n, KnownNat a, KnownNat (ud - 1))
  => ToSchema (TransactionWitness ud n a RollupBFInterpreter)
  where
  declareNamedSchema _ = do
    meRef <- declareSchemaRef (Proxy @(MerkleEntry ud RollupBFInterpreter))
    utxoRef <- declareSchemaRef (Proxy @(UTxO a RollupBFInterpreter))
    rRef <- declareSchemaRef (Proxy @(EdDSAPoint RollupBFInterpreter))
    sRef <- declareSchemaRef (Proxy @(EdDSAScalarField RollupBFInterpreter))
    pkRef <- declareSchemaRef (Proxy @(PublicKey RollupBFInterpreter))
    outsRef <- declareSchemaRef (Proxy @((:.:) (Vector n) (MerkleEntry ud) RollupBFInterpreter))

    let inputSchema =
          Haskell.mempty
            & type_ ?~ OpenApiObject
            & properties
              .~ fromList
                [ ("merkleEntry", meRef)
                , ("utxo", utxoRef)
                , ("r", rRef)
                , ("s", sRef)
                , ("publicKey", pkRef)
                ]
            & required .~ ["merkleEntry", "utxo", "r", "s", "publicKey"]

        inputsSchema =
          Haskell.mempty
            & type_ ?~ OpenApiArray
            & items ?~ OpenApiItemsObject (Inline inputSchema)

        schema =
          Haskell.mempty
            & type_ ?~ OpenApiObject
            & properties
              .~ fromList
                [ ("inputs", Inline inputsSchema)
                , ("outputs", outsRef)
                ]
            & required .~ ["inputs", "outputs"]

    Haskell.pure (named "TransactionWitness" schema)

-- | Validate transaction. See note [State validation] for details.
validateTransaction
  :: forall ud bo n a context
   . SignatureTransaction ud n a context
  => MerkleTree ud context
  -- ^ UTxO tree.
  -> (Vector bo :.: Output a) context
  -- ^ Bridged out outputs.
  -> Transaction n a context
  -- ^ Transaction.
  -> TransactionWitness ud n a context
  -- ^ Transaction witness.
  -> (FieldElement :*: Bool :*: MerkleTree ud) context
  -- ^ Result of validation. First field denotes number of bridged out outputs in this transaction, second one denotes whether the transaction is valid, third one denotes updated UTxO tree.
validateTransaction utxoTree bridgedOutOutputs tx txw =
  let
    txId' = txId tx & Base.hHash
    inputAssets = unComp1 txw.twInputs & Haskell.fmap (\(_ :*: utxo :*: _ :*: _ :*: _) -> unComp1 utxo.uOutput.oAssets)
    outputsAssets = unComp1 tx.outputs & Haskell.fmap (\(output :*: _) -> unComp1 output.oAssets)

    -- Asset-balanced check using random linear combination (Schwartz-Zippel).
    -- For each asset (policy, name, qty), compute qty * ((policy + 1) * r + (name + 1))
    -- and sum across all inputs/outputs. If sums match, the transaction is balanced
    -- with overwhelming probability (soundness error ≤ 1/|F| ≈ 2^{-255}).
    -- The +1 offsets ensure ADA (policy=0, name=0) gets weight r+1 ≠ 0.
    -- r = txId' is bound to the transaction content, so the prover cannot
    -- independently choose r to forge a false balance.
    r = txId'
    weightedSum = foldl' (foldl' (\s av ->
      let qtyFe = toNative (uint av.assetQuantity)
       in s + qtyFe * ((av.assetPolicy + one) * r + (av.assetName + one))))
    sIn = weightedSum (zero :: FieldElement context) inputAssets
    sOut = weightedSum (zero :: FieldElement context) outputsAssets
    isBalanced = sIn == sOut
    inputsWithWitness = zipWith (:*:) (unComp1 tx.inputs) (unComp1 txw.twInputs)
    (isInsValid :*: updatedUTxOTreeForInputs) =
      foldl'
        ( \(isInsValidAcc :*: acc) (inputRef :*: (merkleEntry :*: utxo :*: rPoint :*: s :*: publicKey)) ->
            let
              nullUTxOHash' = nullUTxOHash @a @context
              utxoHash :: HashSimple context = hash utxo & Base.hHash
              (isInTree, updatedTree) =
                MerkleTree.containsAndReplace
                  merkleEntry
                  nullUTxOHash'
                  acc
              isNullUTxO = utxoHash == nullUTxOHash'
              isValid' =
                isInsValidAcc
                  && (inputRef == utxo.uRef)
                  && (utxoHash == MerkleTree.value merkleEntry)
                  && isInTree
                  && ifThenElse
                    isNullUTxO
                    true
                    ( hashFn publicKey
                        == utxo.uOutput.oAddress
                        && eddsaVerify
                          hashFn
                          publicKey
                          txId'
                          (rPoint :*: s)
                    )
             in
              (isValid' :*: updatedTree)
        )
        ((true :: Bool context) :*: utxoTree)
        inputsWithWitness
    outputsWithWitness = zipWith (:*:) (unComp1 tx.outputs) (unComp1 txw.twOutputs)
    (bouts :*: _ :*: outsValid :*: updatedUTxOTreeForOutputs) =
      foldl'
        ( \(boutsAcc :*: outputIx :*: outsValidAcc :*: utxoTreeAcc) ((output :*: bout) :*: merkleEntry) ->
            let isNull = output == nullOutput
                sanity = outputHasValueSanity output
             in ifThenElse
                  bout
                  ( (boutsAcc + one)
                      :*: (outputIx + one)
                      :*: ( outsValidAcc
                              && foldl' (\found boutput -> found || output == boutput) false (unComp1 bridgedOutOutputs)
                              && not isNull
                              && sanity
                          )
                      :*: utxoTreeAcc
                  )
                  ( boutsAcc
                      :*: (outputIx + one)
                      :*: ( let utxo = UTxO {uRef = OutputRef {orTxId = txId', orIndex = outputIx}, uOutput = output}
                                (isInTree, updatedTree) =
                                  MerkleTree.containsAndReplace
                                    merkleEntry
                                    (hash utxo & Base.hHash)
                                    utxoTreeAcc
                             in ( outsValidAcc
                                    && ifThenElse
                                      isNull
                                      true
                                      ( isInTree
                                          && (merkleEntry.value == nullUTxOHash @a @context)
                                          && sanity
                                      )
                                )
                                  :*: ifThenElse
                                    isNull
                                    utxoTreeAcc
                                    updatedTree
                          )
                  )
        )
        (zero :*: zero :*: (true :: Bool context) :*: updatedUTxOTreeForInputs)
        outputsWithWitness
   in
    ( bouts
        :*: (outsValid && isInsValid && isBalanced) -- Note that we don't need to check if transaction consumes at least one input or is null entirely as our transaction currently only has two fields, namely, inputs & outputs and if thus inputs are null, outputs are null too.
        :*: updatedUTxOTreeForOutputs
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
