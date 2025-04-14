{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction.Batch (
  validateTransactionBatch,
) where

import           GHC.Generics                                            (Generic)
import           Prelude                                                 (snd, undefined, ($))

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
import           ZkFold.Symbolic.Ledger.Validation.Transaction.Core      (UTxO)

{- | Common witness for 'Input's validation, i.e., to verify that input belongs to valid UTxO set.

__Note__: Having it as a type synonym helps significantly with compilation times.
-}
type UtxoWitness context =
  -- History of transaction batches, ending at the tip.
  List
    context
    ( TransactionBatch context
    , List
        context
        ( TransactionBatchData context
        , -- We don't require transactions for those batches which did not spend any input belonging to the address of the owner of the output being validated. So, this list may be empty.
          -- We could use 'Maybe' here to denote it but that would likely increase compilation times.
          List context (Transaction context)
        )
    )

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
validateTransactionBatch valBridgeIn valBridgeOut prevTB TransactionBatch {..} tbw@TransactionBatchWitness {..} =
  let (utxos, txOwners, utxosValid) = computeUtxo tbw

      ( -- Batch data hashes as computed via provided witness.
        resBatchAccDataHashes :: List context (DAIndex context, HashSimple context)
        , -- Are individual batches valid? And is 'tbValidityInterval' within the interval of transactions present inside these batches?
          resBatchAccBatchesValid :: Bool context
        , _ :: Interval context
        , _ :: UTxO context
        , _ :: List context (Address context)
        ) =
          Symbolic.List.foldl
            ( Morph
                \( ( batchAccDataHashes :: List s (DAIndex s, HashSimple s)
                    , batchAccBatchesValid :: Bool s
                    , batchAccBatchValidityInterval :: Interval s
                    , batchAccUtxos :: UTxO s
                    , batchAccTxOwners :: List s (Address s)
                    )
                  , (tbd :: TransactionBatchData s, tbdw :: TransactionBatchDataWitness s)
                  ) ->
                    let (batchValid, batchDAIndex) = validateTransactionBatchDataWithIx batchAccBatchValidityInterval tbd tbdw batchAccUtxos batchAccTxOwners
                     in ((batchDAIndex, hasher tbd) Symbolic.List..: batchAccDataHashes, batchAccBatchesValid && batchValid, batchAccBatchValidityInterval, batchAccUtxos, batchAccTxOwners)
            )
            (Symbolic.List.emptyList :: List context (DAIndex context, HashSimple context), true :: Bool context, tbValidityInterval, utxos, txOwners)
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
        && utxosValid

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

-- | Find owners of all spent inputs in the transactions.
transactionOwners :: forall context. Signature context => TransactionBatchWitness context -> List context (Address context)
transactionOwners TransactionBatchWitness {..} =
  Symbolic.List.foldl
    ( Morph \((accBatch :: List s (Address s)), (_ :: TransactionBatchData s, txList :: List s (Transaction s))) ->
        Symbolic.List.foldl
          ( Morph \((accTx :: List s' (Address s')), tx :: Transaction s') ->
              (txOwner tx Symbolic.List..: accTx)
          )
          accBatch
          txList
    )
    (Symbolic.List.emptyList :: List context (Address context))
    (tbwBatchDatas)

computeUtxo :: forall context. Signature context => TransactionBatchWitness context -> (UTxO context, List context (Address context), Bool context)
computeUtxo tbw@TransactionBatchWitness {..} = undefined

-- let
--   -- Find owners of all spent inputs in the transactions.
--   txOwners :: List context (Address context) = transactionOwners tbw
-- in

--   -- Verify that utxo witness contain transaction list in case there is an input with address from 'txOwners'.
--   Symbolic.List.foldl
--     ( Morph \((accBatch :: UTxO s, accBatchValid :: Bool s), (tb :: TransactionBatch s, tbds :: List s (TransactionBatchData s, List s (Transaction s)))) ->
--         let spentInputs :: List s (Input s) = txInputs tx
--          in (spentInputs .: acc, spentInputs)
--     )
--     (emptyList :: UTxO context, true :: Bool context)
--     (tbwUtxoWitness)
