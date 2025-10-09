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
import ZkFold.Data.Vector qualified as Vector
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Algorithm.EdDSA (eddsaVerify)
import ZkFold.Symbolic.Algorithm.Hash.Poseidon qualified as Poseidon
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.Combinators (Iso (..))
import ZkFold.Symbolic.Data.FFA (fromUInt)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (hash)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry, MerkleTree)
import ZkFold.Symbolic.Data.MerkleTree qualified as MerkleTree
import Prelude qualified as P

import ZkFold.Symbolic.Ledger.Types

-- | Transaction witness for validating transaction.
data TransactionWitness ud i o a context = TransactionWitness
  { twInputs :: (Vector i :.: (MerkleEntry ud :*: UTxO a :*: EdDSAPoint :*: EdDSAScalarField :*: EdDSAPoint)) context
  , twOutputs :: (Vector o :.: MerkleEntry ud) context
  }

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
    inputAssets = unComp1 txw.twInputs & P.fmap (\(_me :*: utxo :*: _ :*: _ :*: _) -> utxo.uOutput.oAssets)
    outputsAssets = unComp1 tx.outputs & P.fmap (\(output :*: _isBridgeOut) -> unComp1 output.oAssets)
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
                    ( Poseidon.hash publicKey
                        == utxo.uOutput.oAddress
                        && eddsaVerify
                          ( \rPoint' publicKey' m ->
                              fromUInt
                                ( from
                                    (Poseidon.hash (rPoint' :*: publicKey' :*: m))
                                )
                          )
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
          || (asset.assetPolicy == adaPolicy && asset.assetName == adaName && asset.assetQuantity >= fromConstant @P.Integer 1_000_000)
    )
    false
    (unComp1 (oAssets output))
