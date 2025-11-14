{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.Value (
  AssetPolicy,
  adaPolicy,
  AssetName,
  adaName,
  AssetQuantity,
  AssetValue (..),
  nullAssetValue,
  KnownRegistersAssetQuantity,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic, Generic1)
import ZkFold.Algebra.Class
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Symbolic.Compat (CompatData)
import ZkFold.Symbolic.Data.Combinators (KnownRegisters, RegisterSize (Auto))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Int (Int)
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import ZkFold.Symbolic.Data.V2 (SymbolicData)
import ZkFold.Symbolic.V2 (Symbolic)
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Types.Field (RollupBF)
import ZkFold.Symbolic.Ledger.Types.Orphans ()

-- | Asset policy.
type AssetPolicy context = CompatData FieldElement context

-- | Name of the asset.
type AssetName context = CompatData FieldElement context

-- | Quantity of an asset.
type AssetQuantity context = Int 128 Auto context

type KnownRegistersAssetQuantity a = KnownRegisters a 128 Auto

-- TODO: Replace with actual value, once we finalize how policy names are represented.
adaPolicy :: Symbolic context => AssetPolicy context
adaPolicy = zero

-- TODO: Replace with actual value, once we finalize how asset names are represented.
adaName :: Symbolic context => AssetName context
adaName = zero

-- | A value represents the details of an asset that is contained in a transaction output.
data AssetValue context = AssetValue
  { assetPolicy :: AssetPolicy context
  , assetName :: AssetName context
  , assetQuantity :: AssetQuantity context
  }
  deriving stock (Generic, Generic1, Haskell.Eq, Haskell.Show)
  deriving anyclass SymbolicData

instance Symbolic c => Collect (ConstrainedDatum c) (AssetValue c)

instance
  (KnownRegistersAssetQuantity context, Symbolic context)
  => Eq (AssetValue context)

deriving anyclass instance ToJSON (AssetValue RollupBF)

deriving anyclass instance FromJSON (AssetValue RollupBF)

deriving anyclass instance ToSchema (AssetValue RollupBF)

-- | Null asset value.
nullAssetValue :: Symbolic context => AssetValue context
nullAssetValue =
  AssetValue {assetPolicy = zero, assetName = zero, assetQuantity = zero}

instance Symbolic context => Zero (AssetValue context) where
  zero = nullAssetValue
