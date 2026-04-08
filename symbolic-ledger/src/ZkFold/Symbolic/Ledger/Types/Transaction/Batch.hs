{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.Transaction.Batch (
  TransactionBatch (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic, Generic1)
import GHC.TypeNats (KnownNat)
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.HFunctor.Classes (HShow)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Types.Transaction.Core (Transaction)
import ZkFold.Symbolic.Ledger.Types.Value (KnownRegistersAssetQuantity)

-- | Transaction batch.
newtype TransactionBatch n a t c = TransactionBatch
  { tbTransactions :: Vector t (Transaction n a c)
  -- ^ Vector of transaction hashes.
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

deriving stock instance HShow context => Haskell.Show (TransactionBatch n a t context)

instance
  forall n a t context
   . ( Symbolic context
     , KnownRegistersAssetQuantity context
     )
  => Eq (TransactionBatch n a t context)

deriving anyclass instance forall n a t. ToJSON (TransactionBatch n a t RollupBFInterpreter)

deriving anyclass instance forall n a t. FromJSON (TransactionBatch n a t RollupBFInterpreter)

deriving anyclass instance
  forall n a t
   . (KnownNat n, KnownNat t, KnownNat a) => ToSchema (TransactionBatch n a t RollupBFInterpreter)
