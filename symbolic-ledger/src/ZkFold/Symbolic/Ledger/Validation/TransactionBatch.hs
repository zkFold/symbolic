{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Validation.TransactionBatch (
  TransactionBatchWitness (..),
  validateTransactionBatch,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic, Generic1, (:*:) (..), (:.:) (..))
import GHC.TypeNats (KnownNat, type (-))
import ZkFold.Algebra.Class (Zero (..), one, (+))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq ((==))
import ZkFold.Data.Vector (Vector, Zip (..))
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.MerkleTree (MerkleTree)

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Types.Field (RollupBF)
import ZkFold.Symbolic.Ledger.Validation.Transaction (TransactionWitness, validateTransaction)

-- | Transaction batch witness for validating transaction batch.
newtype TransactionBatchWitness ud i o a t context = TransactionBatchWitness
  { tbwTransactions :: (Vector t :.: TransactionWitness ud i o a) context
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

deriving anyclass instance ToJSON (TransactionBatchWitness ud i o a t RollupBF)

deriving anyclass instance
  forall ud i o a t. (KnownNat i, KnownNat o) => FromJSON (TransactionBatchWitness ud i o a t RollupBF)

deriving anyclass instance
  forall ud i o a t
   . (KnownNat ud, KnownNat i, KnownNat o, KnownNat a, KnownNat t, KnownNat (ud - 1))
  => ToSchema (TransactionBatchWitness ud i o a t RollupBF)

-- | Validate transaction batch. See note [State validation] for details.
validateTransactionBatch
  :: forall ud bo i o a t context
   . SignatureTransactionBatch ud i o a t context
  => MerkleTree ud context
  -- ^ UTxO tree.
  -> (Vector bo :.: Output a) context
  -- ^ Bridged out outputs.
  -> TransactionBatch i o a t context
  -- ^ Transaction batch.
  -> TransactionBatchWitness ud i o a t context
  -- ^ Witness for the transaction batch.
  -> (Bool :*: MerkleTree ud) context
  -- ^ Result of validation. First field denotes whether the transaction batch is valid, second one denotes updated UTxO tree.
validateTransactionBatch utxoTree bridgedOutOutputs tb tbw =
  let
    transactionBatchWithWitness = zipWith (:*:) tb.tbTransactions (unComp1 tbw.tbwTransactions)
    (boCount :*: isValid :*: updatedUTxOTree) =
      foldl'
        ( \(boCountAcc :*: isValidAcc :*: accUTxOTree) (tx :*: txw) ->
            let (txBOuts :*: isTxValid :*: newAccUTxOTree) = validateTransaction accUTxOTree bridgedOutOutputs tx txw
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
