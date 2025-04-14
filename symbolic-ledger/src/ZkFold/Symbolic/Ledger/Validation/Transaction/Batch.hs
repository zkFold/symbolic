{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction.Batch (
  validateTransactionBatch,
) where

import           GHC.Generics                                            (Generic)
import           Prelude                                                 (snd, ($))

import           ZkFold.Symbolic.Data.Bool                               (Bool, BoolType (true), (&&))
import           ZkFold.Symbolic.Data.Class                              (SymbolicData)
import           ZkFold.Symbolic.Data.Conditional                        (Conditional)
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.Eq                                 (Eq, (==))
import           ZkFold.Symbolic.Data.Hash
import qualified ZkFold.Symbolic.Data.List                               as Symbolic.List
import           ZkFold.Symbolic.Data.List                               (List)
import           ZkFold.Symbolic.Data.Morph
import           ZkFold.Symbolic.Ledger.Types
import           ZkFold.Symbolic.Ledger.Validation.Transaction.BatchData
import           ZkFold.Symbolic.Ledger.Validation.Transaction.Core      (UtxoWitness)

-- | Witness for 'TransactionBatch' validation.
data TransactionBatchWitness context = TransactionBatchWitness
  { tbwBatchDatas  :: List context (TransactionBatchData context, TransactionBatchDataWitness context)
  , tbwUtxoWitness :: UtxoWitness context
  }
  deriving stock Generic

instance Signature context => SymbolicData (TransactionBatchWitness context)

instance Signature context => Conditional (Bool context) (TransactionBatchWitness context)

instance Signature context => Eq (TransactionBatchWitness context)

-- | Validate 'TransactionBatch'.
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
  let ( -- Batch data hashes as computed via provided witness.
        resBatchAccDataHashes :: List context (DAIndex context, HashSimple context)
        , -- Are individual batches valid? And is 'tbValidityInterval' within the interval of transactions present inside these batches?
          resBatchAccBatchesValid
        , _
        , _
        ) =
          Symbolic.List.foldl
            ( Morph
                \( ( batchAccDataHashes :: List s (DAIndex s, HashSimple s)
                    , batchAccBatchesValid :: Bool s
                    , batchAccBatchValidityInterval :: Interval s
                    , batchAccUtxoWitness :: UtxoWitness s
                    )
                  , (tbd :: TransactionBatchData s, tbdw :: TransactionBatchDataWitness s)
                  ) ->
                    let (batchValid, batchDAIndex) = validateTransactionBatchDataWithIx batchAccBatchValidityInterval tbd tbdw batchAccUtxoWitness
                     in ((batchDAIndex, hasher tbd) Symbolic.List..: batchAccDataHashes, batchAccBatchesValid && batchValid, batchAccBatchValidityInterval, batchAccUtxoWitness)
            )
            (Symbolic.List.emptyList :: List context (DAIndex context, HashSimple context), true :: Bool context, tbValidityInterval, tbwUtxoWitness)
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
           in (ix Symbolic.List..: accList, curAccNoDuplicateIndices)
      )
      (Symbolic.List.emptyList :: List context (DAIndex context), true :: Bool context)
      ls
