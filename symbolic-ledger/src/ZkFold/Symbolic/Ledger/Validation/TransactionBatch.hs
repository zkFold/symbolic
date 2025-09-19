{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Symbolic.Ledger.Validation.TransactionBatch (
  validateTransactionBatch,
) where

import GHC.Generics ((:*:) (..), (:.:) (..))
import ZkFold.Algebra.Class (Zero (..), one, (+))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq ((==))
import ZkFold.Data.Vector (Vector)
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.MerkleTree (MerkleTree)

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.Transaction (validateTransaction)

validateTransactionBatch
  :: forall ud bo i o a t context
   . SignatureTransactionBatch i o a t context
  => MerkleTree ud context
  -> (Vector bo :.: Output a) context
  -> TransactionBatch i o a t context
  -> (Bool :*: MerkleTree ud) context
validateTransactionBatch utxoTree bridgedOutOutputs tb =
  let (boCount :*: isValid :*: updatedUTxOTree) =
        foldl'
          ( \(boCountAcc :*: isValidAcc :*: accUTxOTree) tx ->
              let (txBOuts :*: isTxValid :*: newAccUTxOTree) = validateTransaction accUTxOTree bridgedOutOutputs tx
               in ((boCountAcc + txBOuts) :*: (isValidAcc && isTxValid) :*: newAccUTxOTree)
          )
          ((zero :: FieldElement context) :*: true :*: utxoTree)
          tb.tbTransactions
      bouts =
        foldl'
          (\acc (output) -> ifThenElse (output == nullOutput @a @context) (acc + one) acc)
          zero
          (unComp1 bridgedOutOutputs)
   in ((isValid && (bouts == boCount)) :*: updatedUTxOTree)
