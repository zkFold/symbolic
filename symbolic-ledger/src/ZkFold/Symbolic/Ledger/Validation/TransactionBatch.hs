{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Symbolic.Ledger.Validation.TransactionBatch (
  TransactionBatchWitness (..),
  validateTransactionBatch,
) where

import GHC.Generics ((:*:) (..), (:.:) (..))
import ZkFold.Algebra.Class (Zero (..), one, (+))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq ((==))
import ZkFold.Data.Vector (Vector, Zip (..))
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.MerkleTree (MerkleTree)
import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.Transaction (TransactionWitness, validateTransaction)

-- | Transaction batch witness for validating transaction batch.
newtype TransactionBatchWitness ud i o a t context = TransactionBatchWitness
  { tbwTransactions :: (Vector t :.: TransactionWitness ud i o a) context
  }

-- | Validate transaction batch. See note [State validation] for details.
validateTransactionBatch
  :: forall ud bo i o a t context
   . SignatureTransactionBatch ud i o a t context
  => TransactionBatchWitness ud i o a t context
  -- ^ Witness for the transaction batch.
  -> MerkleTree ud context
  -- ^ UTxO tree.
  -> (Vector bo :.: Output a) context
  -- ^ Bridged out outputs.
  -> TransactionBatch i o a t context
  -- ^ Transaction batch.
  -> (Bool :*: MerkleTree ud) context
  -- ^ Result of validation. First field denotes whether the transaction batch is valid, second one denotes updated UTxO tree.
validateTransactionBatch tbw utxoTree bridgedOutOutputs tb =
  let
    transactionBatchWithWitness = zipWith (:*:) tb.tbTransactions (unComp1 tbw.tbwTransactions)
    (boCount :*: isValid :*: updatedUTxOTree) =
      foldl'
        ( \(boCountAcc :*: isValidAcc :*: accUTxOTree) (tx :*: txw) ->
            let (txBOuts :*: isTxValid :*: newAccUTxOTree) = validateTransaction txw accUTxOTree bridgedOutOutputs tx
             in ((boCountAcc + txBOuts) :*: (isValidAcc && isTxValid) :*: newAccUTxOTree)
        )
        ((zero :: FieldElement context) :*: true :*: utxoTree)
        transactionBatchWithWitness
    bouts =
      foldl'
        (\acc output -> ifThenElse (output == nullOutput @a @context) acc (acc + one))
        zero
        (unComp1 bridgedOutOutputs)
   in
    ((isValid && (bouts == boCount)) :*: updatedUTxOTree)
