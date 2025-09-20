{-# LANGUAGE DerivingVia #-}

module ZkFold.Protocol.Plonkup.Relation.LookupVector where

import Control.Applicative (Applicative (..), liftA3)
import Data.Functor (Functor, fmap)
import Data.Monoid (Monoid)
import Data.Semialign (Semialign (alignWith))
import Data.Semigroup (Semigroup)
import Data.These (These (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Numeric.Natural (Natural)
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Bool (BoolType)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.Ord (IsOrdering, Ord (..))

data LookupVector a = LV {init :: Vector a, rest :: a}
  deriving Functor
  deriving
    ( AdditiveGroup
    , AdditiveSemigroup
    , BoolType
    , IsOrdering
    , Monoid
    , MultiplicativeMonoid
    , MultiplicativeSemigroup
    , Semigroup
    , Zero
    )
    via (ApplicativeAlgebra LookupVector a)

deriving via
  (ApplicativeAlgebra LookupVector a)
  instance
    FromConstant b a => FromConstant b (LookupVector a)

deriving via
  (ApplicativeAlgebra LookupVector a)
  instance
    Scale k a => Scale k (LookupVector a)

fromVector :: Vector a -> LookupVector a
fromVector init = LV {..} where rest = V.last init

toVector :: Natural -> LookupVector a -> Vector a
toVector (Prelude.fromIntegral -> len) LV {..} =
  V.take len init V.++ V.replicate (len Prelude.- V.length init) rest

instance Applicative LookupVector where
  pure rest = LV {..} where init = V.empty
  LV fi fr <*> LV xi xr = alignWith elim fi xi `LV` fr xr
   where
    elim (This f) = f xr
    elim (That x) = fr x
    elim (These f x) = f x

instance Finite a => Finite (LookupVector a) where
  type Order (LookupVector a) = Order a

instance {-# OVERLAPPING #-} FromConstant (LookupVector a) (LookupVector a)

instance {-# OVERLAPPING #-} FromConstant b a => FromConstant (LookupVector b) (LookupVector a) where
  fromConstant = fmap fromConstant

instance {-# OVERLAPPING #-} MultiplicativeSemigroup a => Scale (LookupVector a) (LookupVector a)

instance Conditional b a => Conditional (LookupVector b) (LookupVector a) where
  bool = liftA3 bool

instance Eq a => Eq (LookupVector a) where
  type BooleanOf (LookupVector a) = LookupVector (BooleanOf a)
  (==) = liftA2 (==)
  (/=) = liftA2 (/=)

instance Ord a => Ord (LookupVector a) where
  type OrderingOf (LookupVector a) = LookupVector (OrderingOf a)
  ordering x y z w = liftA3 ordering x y z <*> w
  compare = liftA2 compare
  (<) = liftA2 (<)
  (<=) = liftA2 (<=)
  (>=) = liftA2 (>=)
  (>) = liftA2 (>)

instance Exponent a p => Exponent (LookupVector a) p where
  a ^ p = fmap (^ p) a

instance AdditiveMonoid a => AdditiveMonoid (LookupVector a)

instance Semiring a => Semiring (LookupVector a)

instance Ring a => Ring (LookupVector a)

instance SemiEuclidean a => SemiEuclidean (LookupVector a) where
  div = liftA2 div
  mod = liftA2 mod

instance Euclidean a => Euclidean (LookupVector a) where
  gcd = liftA2 gcd
  bezoutL = liftA2 bezoutL
  bezoutR = liftA2 bezoutR

instance Field a => Field (LookupVector a) where
  finv = fmap finv

instance PrimeField a => PrimeField (LookupVector a) where
  type IntegralOf (LookupVector a) = LookupVector (IntegralOf a)
  toIntegral = fmap toIntegral
