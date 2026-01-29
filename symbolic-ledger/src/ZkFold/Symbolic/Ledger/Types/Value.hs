{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.Value (
  AssetPolicy,
  adaPolicy,
  AssetName,
  adaName,
  AssetQuantity,
  AssetValue (..),
  nullAssetValue,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic, Generic1)
import Text.Show (Show)
import ZkFold.Algebra.Class
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Int (Int)
import ZkFold.Symbolic.Data.UInt (KnownUInt)
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)

import ZkFold.Symbolic.Ledger.Types.Field (RollupBF)
import ZkFold.Symbolic.Ledger.Types.Orphans ()

-- | Asset policy.
type AssetPolicy context = FieldElement context

-- | Name of the asset.
type AssetName context = FieldElement context

-- | Quantity of an asset.
type AssetQuantity context = Int 64 context

adaPolicy :: Symbolic context => AssetPolicy context
adaPolicy = zero

adaName :: Symbolic context => AssetName context
adaName = zero

-- | A value represents the details of an asset that is contained in a transaction output.
data AssetValue context = AssetValue
  { assetPolicy :: AssetPolicy context
  , assetName :: AssetName context
  , assetQuantity :: AssetQuantity context
  }
  deriving stock (Generic, Generic1, Show)
  deriving anyclass (Eq, SymbolicData, SymbolicInput)

instance Symbolic c => Collect (ConstrainedDatum c) (AssetValue c)

deriving anyclass instance ToJSON (AssetValue RollupBF)

deriving anyclass instance FromJSON (AssetValue RollupBF)

deriving anyclass instance ToSchema (AssetValue RollupBF)

-- | Null asset value.
nullAssetValue :: (Symbolic context, KnownUInt 64 context) => AssetValue context
nullAssetValue =
  AssetValue {assetPolicy = zero, assetName = zero, assetQuantity = zero}

instance (Symbolic c, KnownUInt 64 c) => Zero (AssetValue c) where
  zero = nullAssetValue
