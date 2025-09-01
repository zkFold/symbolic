{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Symbolic.Ledger.Types.Output where

import GHC.Generics (Generic, Generic1)
import ZkFold.Data.Eq (Eq)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

import ZkFold.Symbolic.Ledger.Types.Address (Address)
import ZkFold.Symbolic.Ledger.Types.Datum (Datum)
import ZkFold.Symbolic.Ledger.Types.Value (AssetValue, KnownRegistersAssetQuantity)

-- | Transaction output.
data Output context = Output
  { txoAddress :: Address context
  -- ^ 'Address' at which the value is locked.
  , txoValue :: AssetValue context
  -- ^ 'AssetValue' locked by the output.
  , txoDatum :: Datum context
  -- ^ 'Datum' associated with the output.
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData)

instance (KnownRegistersAssetQuantity context, Symbolic context) => Eq (Output context)
