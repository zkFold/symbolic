{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction.Batch (
  validateTransactionBatch,
) where

import ZkFold.Data.Eq
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (true), (&&))
import ZkFold.Symbolic.Data.Hash
import ZkFold.Symbolic.Data.List (List)
import qualified ZkFold.Symbolic.Data.List as Symbolic.List
import Prelude (($))

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.Transaction.BatchData
import GHC.Generics ((:*:)(..))
import ZkFold.Data.Product (sndP)
import Data.Function ((.))

-- | Witness for 'TransactionBatch' validation.
data TransactionBatchWitness context = TransactionBatchWitness
  { tbwBatchDatas :: List (TransactionBatchData :*: TransactionBatchDataWitness) context
  }

-- | Validate 'TransactionBatch'.
validateTransactionBatch
  :: forall context
   . Signature context
  => TransactionBatch context
  -- ^ Current tip.
  -> AssetValues context
  -- ^ Bridged in assets.
  -> AssetValues context
  -- ^ Bridged out assets.
  -> TransactionBatch context
  -- ^ New batch to be added.
  -> TransactionBatchWitness context
  -- ^ Witness used for validation.
  -> Bool context
validateTransactionBatch valBridgeIn valBridgeOut prevTB TransactionBatch {..} TransactionBatchWitness {..} =
  let -- Batch data hashes as computed via provided witness.
      resBatchAccDataHashes
        :*: -- Are individual batches valid? And is 'tbValidityInterval' within the interval of transactions present inside these batches?
            resBatchAccBatchesValid
        :*: _
         =
          Symbolic.List.foldl
            (\(batchAccDataHashes
                :*: batchAccBatchesValid
                :*: batchAccBatchValidityInterval
              ) (tbd :*: tbdw) ->
                let (batchValid, batchDAIndex) =
                       validateTransactionBatchDataWithIx
                         batchAccBatchValidityInterval tbd tbdw
                 in ((batchDAIndex :*: hasher tbd) Symbolic.List..: batchAccDataHashes)
                    :*: (batchAccBatchesValid && batchValid)
                    :*: batchAccBatchValidityInterval
            )
            (Symbolic.List.emptyList :*: true :*: tbValidityInterval)
            tbwBatchDatas
   in -- 'tbBridgeIn' represents correct hash.
      -- TODO: We might not need to do this check if this is performed by smart contract. Same for 'tbBridgeOut' and 'tbPreviousBatch'
      tbBridgeIn
        == hasher valBridgeIn
        -- 'tbBridgeOut' represents correct hash.
        && tbBridgeOut
        == hasher valBridgeOut
        -- 'tbPreviousBatch' represents correct hash.
        && tbPreviousBatch
        == hasher prevTB
        -- 'tbDataHashes' is consistent with given batches.
        && tbDataHashes
        == resBatchAccDataHashes
        -- There are no entries with duplicate data availability index.
        && noDuplicateIndicesInBatch tbDataHashes
        -- Individual batches are valid and validity interval of batch is within validity interval of batch transactions.
        && resBatchAccBatchesValid

-- TODO: Refactor following once improvements in symbolic list are merged and it supports more utilities like 'elem'.

-- | Check if there are no duplicate 'DAIndex' in the given list.
noDuplicateIndicesInBatch
  :: forall context. Signature context
  => List (DAIndex :*: HashSimple) context -> Bool context
noDuplicateIndicesInBatch =
  sndP .
    Symbolic.List.foldl
      (\(accList :*: accNoDuplicateIndices) (ix :*: _) ->
          let curAccNoDuplicateIndices =
                sndP $
                  Symbolic.List.foldl
                    (\(givenIx :*: accInternalNoDuplicateIndices) ix' ->
                        givenIx :*: (accInternalNoDuplicateIndices && (givenIx /= ix'))
                    )
                    (ix :*: accNoDuplicateIndices)
                    accList
           in (ix Symbolic.List..: accList) :*: curAccNoDuplicateIndices
      )
      (Symbolic.List.emptyList :*: true)
