{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.Transaction.Batch (
  TransactionBatch (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic, Generic1)
import GHC.TypeNats (KnownNat)
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Input (SymbolicInput)

import ZkFold.Symbolic.Ledger.Types.Field (RollupBF)
import ZkFold.Symbolic.Ledger.Types.Transaction.Core (Transaction)

-- | Transaction batch.
newtype TransactionBatch i o a t c = TransactionBatch
  { tbTransactions :: Vector t (Transaction i o a c)
  -- ^ Vector of transaction hashes.
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (Eq, SymbolicData, SymbolicInput)

deriving anyclass instance forall i o a t. ToJSON (TransactionBatch i o a t RollupBF)

deriving anyclass instance forall i o a t. FromJSON (TransactionBatch i o a t RollupBF)

deriving anyclass instance
  forall i o a t
   . (KnownNat i, KnownNat o, KnownNat t, KnownNat a) => ToSchema (TransactionBatch i o a t RollupBF)
