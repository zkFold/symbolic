{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Avoid reduction overflow error caused by NumberOfRegisters
{-# OPTIONS_GHC -freduction-depth=0 #-}

module ZkFold.Symbolic.Cardano.Types.Value where

import qualified Data.Map as Map
import GHC.Natural (Natural)
import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Data.Eq
import ZkFold.Data.HFunctor.Classes (HEq)
import ZkFold.Data.Vector
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators (KnownRegisters, RegisterSize (..))
import ZkFold.Symbolic.Data.Input
import Prelude hiding (Bool, Eq, length, replicate, splitAt, (*), (+))
import qualified Prelude as Haskell

import ZkFold.Symbolic.Cardano.Types.Basic

type PolicyId context = ByteString 224 context

type AssetName context = ByteString 256 context

type SingleAsset context = ((PolicyId context, AssetName context), UInt 64 Auto context)

newtype Value n context = Value {getValue :: Vector n (SingleAsset context)}

deriving instance HEq context => Haskell.Eq (Value n context)

deriving instance
  ( HEq context
  , Haskell.Ord (ByteString 224 context)
  , Haskell.Ord (ByteString 256 context)
  , Haskell.Ord (UInt 64 Auto context)
  )
  => Haskell.Ord (Value n context)

deriving instance (Symbolic context, KnownNat n, KnownRegisters context 64 Auto) => SymbolicData (Value n context)

deriving newtype instance
  ( Symbolic context
  , KnownRegisters context 64 Auto
  )
  => Eq (Value n context)

instance
  ( Symbolic context
  , KnownNat n
  , KnownRegisters context 64 Auto
  )
  => SymbolicInput (Value n context)
  where
  isValid (Value v) = isValid v

instance Symbolic context => Scale Natural (Value n context) where
  scale :: Natural -> Value n context -> Value n context
  n `scale` Value v = Value $ fmap (\((pid, aname), q) -> ((pid, aname), n `scale` q)) v

instance (Haskell.Ord (PolicyId context), Haskell.Ord (AssetName context), Symbolic context) => Semigroup (Value n context) where
  (<>) (Value va) (Value vb) = Value $ unsafeToVector $ Map.toList $ Map.unionWith (+) (Map.fromList (fromVector va)) (Map.fromList (fromVector vb))

instance (Haskell.Ord (PolicyId context), Haskell.Ord (AssetName context), Symbolic context) => Monoid (Value n context) where
  mempty = Value $ unsafeToVector []

instance
  (Haskell.Ord (PolicyId context), Haskell.Ord (AssetName context), Symbolic context)
  => AdditiveSemigroup (Value n context)
  where
  (+) = (<>)

instance (Haskell.Ord (PolicyId context), Haskell.Ord (AssetName context), Symbolic context) => AdditiveMonoid (Value n context) where
  zero = mempty
