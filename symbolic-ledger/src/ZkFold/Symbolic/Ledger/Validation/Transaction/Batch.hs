{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE ImpredicativeTypes #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction.Batch (
  validateTransactionBatch,
) where

import           Prelude                                                 (snd, ($))

import           ZkFold.Symbolic.Data.Bool                               (Bool, BoolType (true), (&&))
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.Hash
import qualified ZkFold.Symbolic.Data.List                               as Symbolic.List
import           ZkFold.Symbolic.Data.List                               (List, (.:))
import           ZkFold.Symbolic.Data.Morph
import           ZkFold.Symbolic.Ledger.Types
import           ZkFold.Symbolic.Ledger.Validation.Transaction.BatchData

data TransactionBatchWitness context = TransactionBatchWitness
  { tbwBatchDatas :: List context (TransactionBatchData context, TransactionBatchDataWitness context)
  }

-- TODO: Add comments.

validateTransactionBatch ::
  forall context.
  Signature context =>
  -- | Current tip.
  TransactionBatch context ->
  -- | Bridged in assets.
  AssetValues context ->
  -- | Bridged out assets.
  AssetValues context ->
  -- | New batch to be added.
  TransactionBatch context ->
  -- | Witness used for validation.
  TransactionBatchWitness context ->
  Bool context
validateTransactionBatch valBridgeIn valBridgeOut prevTB TransactionBatch {..} TransactionBatchWitness {..} =
  let (resBatchAccDataHashes :: List context (DAIndex context, HashSimple context), resBatchAccBatchesValid, _) =
        Symbolic.List.foldl
          ( Morph \((batchAccDataHashes :: List s (DAIndex s, HashSimple s), batchAccBatchesValid :: Bool s, batchAccBatchValidityInterval :: Interval s), (tbd :: TransactionBatchData s, tbdw :: TransactionBatchDataWitness s)) ->
              let (batchValid, batchDAIndex) = validateTransactionBatchData' batchAccBatchValidityInterval tbd tbdw
               in ((batchDAIndex, hasher tbd) Symbolic.List..: batchAccDataHashes, batchAccBatchesValid && batchValid, batchAccBatchValidityInterval)
          )
          (Symbolic.List.emptyList :: List context (DAIndex context, HashSimple context), true :: Bool context, tbValidityInterval)
          tbwBatchDatas
   in tbBridgeIn
        == hasher valBridgeIn
        && tbBridgeOut
        == hasher valBridgeOut
        && tbPreviousBatch
        == hasher prevTB
        && tbDataHashes
        == resBatchAccDataHashes
        && resBatchAccBatchesValid
        && noDuplicateIndicesInBatch tbDataHashes

-- TODO: Refactor this once improvements in symbolic list are merged.

-- | Check if there are no duplicate 'DAIndex' in the given list.
noDuplicateIndicesInBatch :: forall context. Signature context => List context (DAIndex context, HashSimple context) -> Bool context
noDuplicateIndicesInBatch ls =
  snd $
    Symbolic.List.foldl
      ( Morph \((accList :: List s (DAIndex s), accNoDuplicateIndices :: Bool s), (ix :: DAIndex s, _ :: HashSimple s)) ->
          let curAccNoDuplicateIndices :: Bool s =
                snd $
                  Symbolic.List.foldl
                    ( Morph \((givenIx :: DAIndex s', accInternalNoDuplicateIndices :: Bool s'), ix' :: DAIndex s') ->
                        (givenIx, accInternalNoDuplicateIndices && (givenIx /= ix'))
                    )
                    (ix, accNoDuplicateIndices)
                    accList
           in (ix .: accList, curAccNoDuplicateIndices)
      )
      (Symbolic.List.emptyList :: List context (DAIndex context), true :: Bool context)
      ls
