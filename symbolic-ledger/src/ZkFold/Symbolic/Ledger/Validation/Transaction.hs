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
  AdditiveGroup (..),
  AdditiveSemigroup (..),
  FromConstant (fromConstant),
  MultiplicativeMonoid (..),
  Zero (..),
 )
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq
import ZkFold.Data.HFunctor.Classes (HShow)
import ZkFold.Data.Ord ((>=))
import ZkFold.Data.Vector (Vector, Zip (..), (!!), fromVector)
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
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry (..), MerkleTree)
import ZkFold.Symbolic.Data.MerkleTree qualified as MerkleTree
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
data TransactionWitness ud i o a context = TransactionWitness
  { twInputs :: (Vector i :.: (MerkleEntry ud :*: UTxO a :*: EdDSAPoint :*: EdDSAScalarField :*: PublicKey)) context
  , twOutputs :: (Vector o :.: MerkleEntry ud) context
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

deriving stock instance HShow context => Haskell.Show (TransactionWitness ud i o a context)

instance ToJSON (TransactionWitness ud i o a RollupBFInterpreter) where
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

instance (KnownNat i, KnownNat o) => FromJSON (TransactionWitness ud i o a RollupBFInterpreter) where
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
-- >>> BSL.putStrLn $ encodePretty $ toSchema (Proxy :: Proxy (TransactionWitness 2 1 1 1 RollupBFInterpreter))
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
  forall ud i o a
   . (KnownNat ud, KnownNat i, KnownNat o, KnownNat a, KnownNat (ud - 1))
  => ToSchema (TransactionWitness ud i o a RollupBFInterpreter)
  where
  declareNamedSchema _ = do
    meRef <- declareSchemaRef (Proxy @(MerkleEntry ud RollupBFInterpreter))
    utxoRef <- declareSchemaRef (Proxy @(UTxO a RollupBFInterpreter))
    rRef <- declareSchemaRef (Proxy @(EdDSAPoint RollupBFInterpreter))
    sRef <- declareSchemaRef (Proxy @(EdDSAScalarField RollupBFInterpreter))
    pkRef <- declareSchemaRef (Proxy @(PublicKey RollupBFInterpreter))
    outsRef <- declareSchemaRef (Proxy @((:.:) (Vector o) (MerkleEntry ud) RollupBFInterpreter))

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
  :: forall ud bo i o a context
   . SignatureTransaction ud i o a context
  => MerkleTree ud context
  -- ^ UTxO tree.
  -> (Vector bo :.: Output a) context
  -- ^ Bridged out outputs.
  -> Transaction i o a context
  -- ^ Transaction.
  -> TransactionWitness ud i o a context
  -- ^ Transaction witness.
  -> (FieldElement :*: Bool :*: MerkleTree ud) context
  -- ^ Result of validation. First field denotes number of bridged out outputs in this transaction, second one denotes whether the transaction is valid, third one denotes updated UTxO tree.
validateTransaction utxoTree bridgedOutOutputs tx txw =
  let
    txId' = txId tx & Base.hHash
    -- Precompute nullUTxOHash once to avoid redundant computation
    nullUTxOHash' = nullUTxOHash @a @context
    
    inputAssets = unComp1 txw.twInputs & Haskell.fmap (\(_me :*: utxo :*: _ :*: _ :*: _) -> utxo.uOutput.oAssets)
    outputsAssets = unComp1 tx.outputs & Haskell.fmap (\(output :*: _isBridgeOut) -> unComp1 output.oAssets)
    
    -- Flatten all input assets into a single list for efficient processing
    allInputAssets = foldl' (\acc comp -> acc Haskell.<> fromVector (unComp1 comp)) [] inputAssets
    allOutputAssets = foldl' (\acc vec -> acc Haskell.<> fromVector vec) [] outputsAssets
    
    -- Optimized asset balancing: For each output asset, compute input sum and output sum
    -- Check that input sum == output sum for proper conservation
    -- This computes sums once per unique (policy, name) pair encountered in outputs
    
    -- For a given asset (policy, name), compute total input and output quantities
    computeBalance refAsset =
      let inputSum = foldl'
            ( \s av ->
                ifThenElse
                  ((av.assetPolicy == refAsset.assetPolicy) && (av.assetName == refAsset.assetName))
                  (s + av.assetQuantity)
                  s
            )
            zero
            allInputAssets
          outputSum = foldl'
            ( \s av ->
                ifThenElse
                  ((av.assetPolicy == refAsset.assetPolicy) && (av.assetName == refAsset.assetName))
                  (s + av.assetQuantity)
                  s
            )
            zero
            allOutputAssets
       in inputSum == outputSum
    
    -- Check balance for each output asset (covers all asset types that appear in outputs)
    outAssetsBalanced =
      foldl' (\acc av -> acc && computeBalance av) (true :: Bool context) allOutputAssets
    
    -- Check that any input asset not in outputs has zero quantity
    -- (handles assets that appear only in inputs)
    inputsFullyAccounted =
      foldl'
        ( \acc av ->
            let hasMatchingOutput = foldl'
                  ( \found oav ->
                      found || ((av.assetPolicy == oav.assetPolicy) && (av.assetName == oav.assetName))
                  )
                  false
                  allOutputAssets
             in acc && (hasMatchingOutput || av.assetQuantity == zero)
        )
        true
        allInputAssets
    
    -- Combined asset validity check  
    assetsBalanced = outAssetsBalanced && inputsFullyAccounted
    
    inputsWithWitness = zipWith (:*:) (unComp1 tx.inputs) (unComp1 txw.twInputs)
    (isInsValid :*: consumedAtleastOneInput :*: updatedUTxOTreeForInputs) =
      foldl'
        ( \(isInsValidAcc :*: consumedAtleastOneAcc :*: acc) (inputRef :*: (merkleEntry :*: utxo :*: rPoint :*: s :*: publicKey)) ->
            let
              utxoHash :: HashSimple context = hash utxo & Base.hHash
              -- Precompute address once
              expectedAddress = hashFn publicKey
              -- Note: The contains check is now done via replaceVerified's internal assertion
              isValid' =
                isInsValidAcc
                  && (inputRef == utxo.uRef)
                  && (utxoHash == MerkleTree.value merkleEntry)
                  && ifThenElse
                    (utxoHash == nullUTxOHash')
                    true
                    ( expectedAddress == utxo.uOutput.oAddress
                        -- && eddsaVerify
                        --   hashFn
                        --   publicKey
                        --   txId'
                        --   (rPoint :*: s)
                    )
             in
              ( isValid'
                  :*: (consumedAtleastOneAcc || (utxoHash /= nullUTxOHash'))
                  -- Use replaceVerified for O(log n) root computation instead of O(n)
                  :*: MerkleTree.replaceVerified merkleEntry nullUTxOHash' acc
              )
        )
        ((true :: Bool context) :*: (false :: Bool context) :*: utxoTree)
        inputsWithWitness
    outputsWithWitness = zipWith (:*:) (unComp1 tx.outputs) (unComp1 txw.twOutputs)
    (bouts :*: _ :*: outsValid :*: updatedUTxOTreeForOutputs) =
      foldl'
        ( \(boutsAcc :*: outputIx :*: outsValidAcc :*: utxoTreeAcc) ((output :*: bout) :*: merkleEntry) ->
            ifThenElse
              bout
              ( (boutsAcc + one)
                  :*: (outputIx + one)
                  :*: ( outsValidAcc
                          && foldl' (\found boutput -> found || output == boutput) false (unComp1 bridgedOutOutputs)
                          && (output /= nullOutput)
                          && outputHasValueSanity output
                      )
                  :*: utxoTreeAcc
              )
              ( boutsAcc
                  :*: (outputIx + one)
                  :*: ( outsValidAcc
                          && ifThenElse
                            (output == nullOutput)
                            true
                            ( -- Note: The contains check is now done via replaceVerified's internal assertion
                              (merkleEntry.value == nullUTxOHash')
                                && outputHasValueSanity output
                            )
                      )
                  :*: ( let utxo = UTxO {uRef = OutputRef {orTxId = txId', orIndex = outputIx}, uOutput = output}
                            newUtxoHash = hash utxo & Base.hHash
                         in ifThenElse
                              (output == nullOutput)
                              utxoTreeAcc
                              -- Use replaceVerified for O(log n) root computation instead of O(n)
                              (MerkleTree.replaceVerified merkleEntry newUtxoHash utxoTreeAcc)
                      )
              )
        )
        (zero :*: zero :*: (true :: Bool context) :*: updatedUTxOTreeForInputs)
        outputsWithWitness
   in
    ( bouts
        :*: (outsValid && isInsValid && consumedAtleastOneInput && assetsBalanced)
        :*: updatedUTxOTreeForOutputs
    )

-- | Check if output has sane value.
--
-- We check if the output has at least one ada and all assets are non-negative.
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
                :*: (allNonNegAcc && (asset.assetQuantity >= zero))
          )
          (false :*: true)
          (unComp1 (oAssets output))
   in adaCheck && allNonNegCheck
