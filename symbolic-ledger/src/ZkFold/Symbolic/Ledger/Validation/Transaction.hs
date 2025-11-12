{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction (
  TransactionWitness (..),
  validateTransaction,
  outputHasAtLeastOneAda,
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
import ZkFold.Data.Vector (Vector, Zip (..), (!!))
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
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry, MerkleTree)
import ZkFold.Symbolic.Data.MerkleTree qualified as MerkleTree
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Types.Orphans ()
import ZkFold.Symbolic.Ledger.Utils (unsafeToVector')

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
    inputAssets = unComp1 txw.twInputs & Haskell.fmap (\(_me :*: utxo :*: _ :*: _ :*: _) -> utxo.uOutput.oAssets)
    outputsAssets = unComp1 tx.outputs & Haskell.fmap (\(output :*: _isBridgeOut) -> unComp1 output.oAssets)
    -- We check if all output assets are covered by inputs.
    (outAssetsWithinInputs :*: finalInputAssets) =
      foldl'
        ( \(isValid1 :*: inputAssetsAcc) outputAssets ->
            foldl'
              ( \(isValid2 :*: inputAssetsAcc') outputAsset ->
                  let
                    -- Whether an input asset matches the current output asset by policy and name.
                    sameAsset av = (av.assetPolicy == outputAsset.assetPolicy) && (av.assetName == outputAsset.assetName)

                    -- Given a remaining quantity to cover, compute the remaining after consuming from one input's assets.
                    remainingAfterInput rem inAssetsComp =
                      let asVec = unComp1 inAssetsComp
                       in foldl'
                            ( \r av ->
                                ifThenElse
                                  (sameAsset av)
                                  ( ifThenElse
                                      (r >= av.assetQuantity)
                                      (r + negate av.assetQuantity)
                                      zero
                                  )
                                  r
                            )
                            rem
                            asVec

                    -- Compute remaining before each input using a prefix scan across inputs.
                    inputsVec = unComp1 inputAssetsAcc'
                    remsAcrossInputs =
                      Vector.scanl
                        remainingAfterInput
                        outputAsset.assetQuantity
                        inputsVec

                    -- Update assets for a single input, given the remaining before this input.
                    updateInputAssets remBefore inAssetsComp =
                      let asVec = unComp1 inAssetsComp
                          -- Remaining before each asset within this input.
                          remsWithinInput =
                            Vector.scanl
                              ( \r av ->
                                  ifThenElse
                                    (sameAsset av)
                                    ( ifThenElse
                                        (r >= av.assetQuantity)
                                        (r + negate av.assetQuantity)
                                        zero
                                    )
                                    r
                              )
                              remBefore
                              asVec
                          -- Update each asset using the remaining before that asset.
                          updatedAs =
                            Vector.mapWithIx
                              ( \ix av ->
                                  let rBefore = remsWithinInput !! ix
                                   in ifThenElse
                                        (sameAsset av)
                                        ( let newQty = ifThenElse (rBefore >= av.assetQuantity) zero (av.assetQuantity + negate rBefore)
                                           in av {assetQuantity = newQty}
                                        )
                                        av
                              )
                              asVec
                       in Comp1 updatedAs

                    -- Build updated inputs by mapping with index and using prefix-remaining per input.
                    updatedInputs =
                      Vector.mapWithIx
                        ( \ix inAssetsComp ->
                            let remBefore = remsAcrossInputs !! ix
                             in updateInputAssets remBefore inAssetsComp
                        )
                        inputsVec

                    -- Final remaining after all inputs processed for this output asset.
                    finalRemaining = Vector.last remsAcrossInputs
                    isValid' = isValid2 && (finalRemaining == zero)
                   in
                    (isValid' :*: Comp1 updatedInputs)
              )
              (isValid1 :*: inputAssetsAcc)
              outputAssets
        )
        ((true :: Bool context) :*: Comp1 inputAssets)
        outputsAssets
    -- We check if all inputs are covered by outputs.
    inputsConsumed =
      foldl'
        ( \acc inAssetsComp ->
            let asVec = unComp1 inAssetsComp
             in acc
                  && foldl'
                    (\acc2 av -> acc2 && (av.assetQuantity == zero))
                    true
                    asVec
        )
        true
        (unComp1 finalInputAssets)
    inputsWithWitness = zipWith (:*:) (unComp1 tx.inputs) (unComp1 txw.twInputs)
    (isInsValid :*: consumedAtleastOneInput :*: updatedUTxOTreeForInputs) =
      foldl'
        ( \(isInsValidAcc :*: consumedAtleastOneAcc :*: acc) (inputRef :*: (merkleEntry :*: utxo :*: rPoint :*: s :*: publicKey)) ->
            let
              nullUTxOHash' = nullUTxOHash @a @context
              utxoHash :: HashSimple context = hash utxo & Base.hHash
              isValid' =
                isInsValidAcc
                  && (inputRef == utxo.uRef)
                  && (utxoHash == MerkleTree.value merkleEntry)
                  && (acc `MerkleTree.contains` merkleEntry)
                  && ifThenElse
                    (utxoHash == nullUTxOHash')
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
              ( isValid'
                  :*: (consumedAtleastOneAcc || (utxoHash /= nullUTxOHash'))
                  :*: MerkleTree.replace
                    ( merkleEntry
                        { MerkleTree.value = nullUTxOHash'
                        }
                    )
                    acc
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
                      )
                  :*: utxoTreeAcc
              )
              ( boutsAcc
                  :*: (outputIx + one)
                  :*: ( outsValidAcc
                          && ifThenElse
                            (output == nullOutput)
                            true
                            ( (utxoTreeAcc `MerkleTree.contains` merkleEntry)
                                && (merkleEntry.value == nullUTxOHash @a @context)
                                && outputHasAtLeastOneAda output
                            )
                      )
                  :*: ( let utxo = UTxO {uRef = OutputRef {orTxId = txId', orIndex = outputIx}, uOutput = output}
                         in ifThenElse
                              (output == nullOutput)
                              utxoTreeAcc
                              ( MerkleTree.replace
                                  ( merkleEntry
                                      { MerkleTree.value = hash utxo & Base.hHash
                                      }
                                  )
                                  utxoTreeAcc
                              )
                      )
              )
        )
        (zero :*: zero :*: (true :: Bool context) :*: updatedUTxOTreeForInputs)
        outputsWithWitness
   in
    ( bouts
        :*: (outsValid && isInsValid && consumedAtleastOneInput && outAssetsWithinInputs && inputsConsumed)
        :*: updatedUTxOTreeForOutputs
    )

-- | Check if output has at least one ada.
outputHasAtLeastOneAda
  :: forall a context
   . (KnownRegistersAssetQuantity context, Symbolic context)
  => Output a context
  -> Bool context
outputHasAtLeastOneAda output =
  foldl'
    ( \found asset ->
        found
          || ( asset.assetPolicy
                 == adaPolicy
                 && asset.assetName
                 == adaName
                 && asset.assetQuantity
                 >= fromConstant @Haskell.Integer 1_000_000
             )
    )
    false
    (unComp1 (oAssets output))
