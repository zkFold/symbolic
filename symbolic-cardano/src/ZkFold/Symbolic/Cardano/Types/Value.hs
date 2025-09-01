{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- Avoid reduction overflow error caused by NumberOfRegisters
{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# LANGUAGE BlockArguments #-}

module ZkFold.Symbolic.Cardano.Types.Value where

import qualified Data.Map as Map
import GHC.Generics (Generic1, Generic)
import GHC.Natural (Natural)
import ZkFold.Algebra.Class
import ZkFold.Data.Eq
import ZkFold.Data.HFunctor.Classes (HEq)
import ZkFold.Data.Vector
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators (RegisterSize (..))
import ZkFold.Symbolic.Data.Input
import Prelude hiding (Bool, Eq, length, replicate, splitAt, (*), (+))
import qualified Prelude as Haskell

import ZkFold.Symbolic.Cardano.Types.Basic

type PolicyId = ByteString 224

type AssetName = ByteString 256

data SingleAsset context = SingleAsset
  { policyId :: PolicyId context
  , assetName :: AssetName context
  , amount :: UInt 64 Auto context
  }
  deriving (Generic, Generic1, SymbolicData, SymbolicInput)

deriving instance HEq context => Haskell.Eq (SingleAsset context)

deriving instance Symbolic context => Eq (SingleAsset context)

deriving instance
  ( HEq context
  , Haskell.Ord (PolicyId context)
  , Haskell.Ord (AssetName context)
  , Haskell.Ord (UInt 64 Auto context)
  ) => Haskell.Ord (SingleAsset context)

instance Symbolic context => Scale Natural (SingleAsset context) where
  scale k SingleAsset {..} = SingleAsset { amount = scale k amount, .. }

newtype Value n context = Value {getValue :: Vector n (SingleAsset context)}
  deriving stock Generic1
  deriving anyclass (SymbolicData, SymbolicInput)

deriving instance HEq context => Haskell.Eq (Value n context)

deriving instance
  ( HEq context
  , Haskell.Ord (ByteString 224 context)
  , Haskell.Ord (ByteString 256 context)
  , Haskell.Ord (UInt 64 Auto context)
  )
  => Haskell.Ord (Value n context)

deriving newtype instance Symbolic context => Eq (Value n context)

instance Symbolic context => Scale Natural (Value n context) where
  scale :: Natural -> Value n context -> Value n context
  n `scale` Value v = Value $ fmap (scale n) v

instance (HEq context, Haskell.Ord (PolicyId context), Haskell.Ord (AssetName context), Symbolic context) => Semigroup (Value n context) where
  Value va <> Value vb =
    Value $ fromMap $ Map.unionWith (+) (toMap va) (toMap vb)
    where
      toMap = Map.fromList . fromVector . fmap
        \(SingleAsset pid aname q) -> ((pid, aname), q)
      fromMap =
        fmap (\((pid, aname), q) -> SingleAsset pid aname q)
        . unsafeToVector . Map.toList

instance (HEq context, Haskell.Ord (PolicyId context), Haskell.Ord (AssetName context), Symbolic context) => Monoid (Value n context) where
  mempty = zero

instance
  (HEq context, Haskell.Ord (PolicyId context), Haskell.Ord (AssetName context), Symbolic context)
  => AdditiveSemigroup (Value n context)
  where
  (+) = (<>)

instance Zero (Value n context) where
  zero = Value $ unsafeToVector []

instance (HEq context, Haskell.Ord (PolicyId context), Haskell.Ord (AssetName context), Symbolic context) => AdditiveMonoid (Value n context)
