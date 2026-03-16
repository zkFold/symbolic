{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Symbolic.Ledger.Validation.TransactionBatch (
  validateTransactionBatch,
) where

import ZkFold.Data.Vector (Vector)
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.Transaction (validateTransaction)

validateTransactionBatch
  :: forall context t bo
   . SignatureTransactionBatch context t
  => AccountInfo context
  -> Vector bo (Address context, Address context, AssetValue context)
  -> TransactionBatch context t
  -> (Bool context, AccountInfo context, AccountInfo context)
validateTransactionBatch ai bridgedOutAssets tb =
  foldl'
    ( \(isValid, aiAcc, aiAccWithoutBridgedOut) tx ->
        let (isTxValid, newAi, newAiWithoutBridgedOut) = validateTransaction aiAcc aiAccWithoutBridgedOut bridgedOutAssets tx
         in (isValid && isTxValid, newAi, newAiWithoutBridgedOut)
    )
    (true :: Bool context, ai, ai)
    tb.tbTransactions
