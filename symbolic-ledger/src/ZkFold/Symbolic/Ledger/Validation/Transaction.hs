{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction (
  TransactionWitness (..),
  validateTransaction,
  outputHasAtLeastOneAda,
) where

import Data.Function ((&))
import GHC.Generics ((:*:) (..), (:.:) (..))
import ZkFold.Algebra.Class (
  AdditiveGroup (..),
  AdditiveSemigroup (..),
  FromConstant (fromConstant),
  MultiplicativeMonoid (..),
  Zero (..),
 )
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq
import ZkFold.Data.Ord ((>=))
import ZkFold.Data.Vector (Vector, Zip (..), (!!))
import qualified ZkFold.Data.Vector as Vector
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (hash)
import qualified ZkFold.Symbolic.Data.Hash as Base
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry, MerkleTree)
import qualified ZkFold.Symbolic.Data.MerkleTree as MerkleTree
import qualified Prelude as P

import ZkFold.Symbolic.Ledger.Types

data TransactionWitness ud i o a context = TransactionWitness
  { twInputs :: (Vector i :.: (MerkleEntry ud :*: UTxO a)) context
  , twOutputs :: (Vector o :.: MerkleEntry ud) context
  }

validateTransaction
  :: forall ud bo i o a context
   . SignatureTransaction ud i o a context
  => TransactionWitness ud i o a context
  -> MerkleTree ud context
  -> (Vector bo :.: Output a) context
  -> Transaction i o a context
  -> (FieldElement :*: Bool :*: MerkleTree ud) context
validateTransaction txw utxoTree bridgedOutOutputs tx =
  let
    txId' = txId tx & Base.hHash
    inputAssets = unComp1 txw.twInputs & P.fmap (\(_me :*: utxo) -> utxo.uOutput.oAssets)
    outputsAssets = unComp1 tx.outputs & P.fmap (\(output :*: _isBridgeOut) -> unComp1 output.oAssets)
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
    (isInsValid :*: updatedUTxOTreeForInputs) =
      foldl'
        ( \(isInsValidAcc :*: acc) (inputRef :*: (merkleEntry :*: utxo)) ->
            let
              utxoHash :: HashSimple context = hash utxo & Base.hHash
              isValid' =
                isInsValidAcc
                  && (inputRef == utxo.uRef)
                  && (utxoHash == MerkleTree.value merkleEntry)
                  && (acc `MerkleTree.contains` merkleEntry)
                  && (utxo /= nullUTxO)
             in
              ( isValid'
                  :*: MerkleTree.replace
                    ( merkleEntry
                        { MerkleTree.value = nullUTxOHash @a @context
                        }
                    )
                    acc
              )
        )
        ((true :: Bool context) :*: utxoTree)
        inputsWithWitness
    outputsWithWitness = zipWith (:*:) (unComp1 tx.outputs) (unComp1 txw.twOutputs)
    (bouts :*: _ :*: boutsValid :*: updatedUTxOTreeForOutputs) =
      foldl'
        ( \(boutsAcc :*: outputIx :*: boutsValidAcc :*: utxoTreeAcc) ((output :*: bout) :*: merkleEntry) ->
            ifThenElse
              bout
              ( (boutsAcc + one)
                  :*: (outputIx + one)
                  :*: ( boutsValidAcc
                          && foldl' (\found boutput -> found || output == boutput) false (unComp1 bridgedOutOutputs)
                      )
                  :*: utxoTreeAcc
              )
              ( boutsAcc
                  :*: (outputIx + one)
                  :*: ( boutsValidAcc
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
    (bouts :*: (boutsValid && isInsValid && outAssetsWithinInputs && inputsConsumed) :*: updatedUTxOTreeForOutputs)

outputHasAtLeastOneAda
  :: forall a context
   . (KnownRegistersAssetQuantity context, Symbolic context)
  => Output a context
  -> Bool context
outputHasAtLeastOneAda output =
  foldl'
    ( \found asset ->
        found
          || (asset.assetPolicy == adaPolicy && asset.assetName == adaName && asset.assetQuantity >= fromConstant @P.Integer 1_000_000)
    )
    false
    (unComp1 (oAssets output))
