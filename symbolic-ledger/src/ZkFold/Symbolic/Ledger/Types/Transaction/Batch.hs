{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.Transaction.Batch (
  TransactionBatch (..),
) where

import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat)
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Ledger.Types.Transaction.Core (Transaction)
import ZkFold.Symbolic.Ledger.Types.Value (KnownRegistersAssetQuantity)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

newtype TransactionBatch c t = TransactionBatch
  { tbTransactions :: Vector t (Transaction c)
  -- ^ Vector of transaction hashes.
  }
  deriving stock Generic

instance
  ( Symbolic context
  , KnownRegistersAssetQuantity context
  , KnownNat t
  )
  => SymbolicData (TransactionBatch context t)

instance
  ( Symbolic context
  , KnownRegistersAssetQuantity context
  , KnownNat t
  )
  => Eq (TransactionBatch context t)
