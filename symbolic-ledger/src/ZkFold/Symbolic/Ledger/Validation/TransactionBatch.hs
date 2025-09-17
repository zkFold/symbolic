{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Symbolic.Ledger.Validation.TransactionBatch (
  validateTransactionBatch,
) where

import GHC.Generics ((:*:) (..), (:.:))
import ZkFold.Data.Vector (Vector)
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.Transaction (validateTransaction)

validateTransactionBatch
  :: forall context t bo
   . SignatureTransactionBatch context t
  => AccountInfo context
  -> (Vector bo :.: (Address :*: Address :*: AssetValue)) context
  -> TransactionBatch t context
  -> (Bool :*: AccountInfo :*: AccountInfo) context
validateTransactionBatch ai bridgedOutAssets tb =
  foldl'
    ( \(isValid :*: aiAcc :*: aiAccWithoutBridgedOut) tx ->
        let (isTxValid :*: newAi :*: newAiWithoutBridgedOut) = validateTransaction aiAcc aiAccWithoutBridgedOut bridgedOutAssets tx
         in ((isValid && isTxValid) :*: newAi :*: newAiWithoutBridgedOut)
    )
    (true :*: ai :*: ai)
    tb.tbTransactions