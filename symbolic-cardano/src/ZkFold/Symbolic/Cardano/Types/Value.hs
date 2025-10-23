{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Cardano.Types.Value where

import qualified Data.Map as Map
import GHC.Generics (Generic, Generic1)
import GHC.Natural (Natural)
import ZkFold.Algebra.Class
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq
import ZkFold.Data.Vector
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.UInt (KnownUInt)
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import Prelude hiding (Bool, Eq, length, replicate, splitAt, (*), (+))
import qualified Prelude as Haskell

import ZkFold.Symbolic.Cardano.Types.Basic

type PolicyId = ByteString 224

type AssetName = ByteString 256

data SingleAsset context = SingleAsset
  { policyId :: PolicyId context
  , assetName :: AssetName context
  , amount :: UInt 64 context
  }
  deriving (Eq, Generic, Generic1, SymbolicData)

deriving instance Symbolic c => Collect (ConstrainedDatum c) (SingleAsset c)

instance (Symbolic c, KnownUInt 64 c) => Scale Natural (SingleAsset c) where
  scale k SingleAsset {..} = SingleAsset {amount = scale k amount, ..}

newtype Value n context = Value {getValue :: Vector n (SingleAsset context)}
  deriving stock (Generic, Generic1)
  deriving anyclass (Eq, SymbolicData)

instance Symbolic c => Collect (ConstrainedDatum c) (Value n c)

instance (Symbolic c, KnownUInt 64 c) => Scale Natural (Value n c) where
  n `scale` Value v = Value $ fmap (scale n) v

instance
  ( Haskell.Eq c
  , Haskell.Ord (PolicyId c)
  , Haskell.Ord (AssetName c)
  , Symbolic c
  , KnownUInt 64 c
  )
  => Semigroup (Value n c)
  where
  Value va <> Value vb =
    Value $ fromMap $ Map.unionWith (+) (toMap va) (toMap vb)
   where
    toMap =
      Map.fromList . fromVector . fmap
        \(SingleAsset pid aname q) -> ((pid, aname), q)
    fromMap =
      fmap (\((pid, aname), q) -> SingleAsset pid aname q)
        . unsafeToVector
        . Map.toList

instance
  ( Haskell.Eq c
  , Haskell.Ord (PolicyId c)
  , Haskell.Ord (AssetName c)
  , Symbolic c
  , KnownUInt 64 c
  )
  => Monoid (Value n c)
  where
  mempty = zero

instance
  ( Haskell.Eq c
  , Haskell.Ord (PolicyId c)
  , Haskell.Ord (AssetName c)
  , Symbolic c
  , KnownUInt 64 c
  )
  => AdditiveSemigroup (Value n c)
  where
  (+) = (<>)

instance Zero (Value n context) where
  zero = Value $ unsafeToVector []

instance
  ( Haskell.Eq c
  , Haskell.Ord (PolicyId c)
  , Haskell.Ord (AssetName c)
  , Symbolic c
  , KnownUInt 64 c
  )
  => AdditiveMonoid (Value n c)
