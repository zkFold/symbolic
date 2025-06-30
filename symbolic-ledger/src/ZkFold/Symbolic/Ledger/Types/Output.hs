{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.Output where

import GHC.Generics (Generic)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Data.Eq (Eq)
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
  deriving stock Generic

instance (KnownRegistersAssetQuantity context, Symbolic context) => SymbolicData (Output context)

instance (KnownRegistersAssetQuantity context, Symbolic context) => Eq (Output context)
