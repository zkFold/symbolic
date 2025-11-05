{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.Transaction.Batch (
  TransactionBatch (..),
) where

import GHC.Generics (Generic, Generic1)
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))

import ZkFold.Symbolic.Ledger.Types.Transaction.Core (Transaction)
import ZkFold.Symbolic.Ledger.Types.Value (KnownRegistersAssetQuantity)
import ZkFold.Symbolic.Data.Input (SymbolicInput)

-- | Transaction batch.
newtype TransactionBatch i o a t c = TransactionBatch
  { tbTransactions :: Vector t (Transaction i o a c)
  -- ^ Vector of transaction hashes.
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

instance
  forall i o a t context
   . ( Symbolic context
     , KnownRegistersAssetQuantity context
     )
  => Eq (TransactionBatch i o a t context)
