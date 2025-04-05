{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE ImpredicativeTypes #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction.Batch (

) where

import           Prelude                                                 (undefined)

import           ZkFold.Symbolic.Data.Bool                               (Bool, BoolType (true), (&&))
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.Hash
import qualified ZkFold.Symbolic.Data.List                               as Symbolic.List
import           ZkFold.Symbolic.Data.List                               (List)
import           ZkFold.Symbolic.Data.Morph
import           ZkFold.Symbolic.Ledger.Types
import           ZkFold.Symbolic.Ledger.Validation.Transaction.BatchData

data TransactionBatchWitness context = TransactionBatchWitness
  { tbwBatchDatas :: List context (TransactionBatchData context, TransactionBatchDataWitness context)
  }

-- TODO: Add comments.
-- TODO: Check that there are no duplicates in tbDataHashes.

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
