{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.Transaction.Batch (
  TransactionBatch (..),
) where

import GHC.Generics (Generic, Generic1)
import GHC.TypeNats (KnownNat)
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))

import ZkFold.Symbolic.Ledger.Types.Transaction.Core (Transaction)
import ZkFold.Symbolic.Ledger.Types.Value (KnownRegistersAssetQuantity)

newtype TransactionBatch t c = TransactionBatch
  { tbTransactions :: Vector t (Transaction c)
  -- ^ Vector of transaction hashes.
  }
  deriving stock (Generic, Generic1)
  deriving anyclass SymbolicData

instance
  ( Symbolic context
  , KnownRegistersAssetQuantity context
  , KnownNat t
  )
  => Eq (TransactionBatch t context)
