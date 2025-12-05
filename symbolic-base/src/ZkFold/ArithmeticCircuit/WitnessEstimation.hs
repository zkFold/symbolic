{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.ArithmeticCircuit.WitnessEstimation where

import Control.Applicative (Applicative (..), liftA3)
import Data.Bool (Bool (..), otherwise)
import Data.Eq (Eq, (==))
import Data.Function ((.))
import Data.Functor (Functor, fmap)
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Type.Equality (type (~))
import GHC.Real (Integral)

import ZkFold.Algebra.Class
import ZkFold.ArithmeticCircuit.Var (NewVar)
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Bool
import ZkFold.Data.Eq (BooleanOf)
import qualified ZkFold.Data.Eq as ZkFold
import ZkFold.Data.Ord (IsOrdering (..), Ord (..))

data UVar a = ConstUVar a | LinUVar a NewVar a | More deriving Functor

instance {-# INCOHERENT #-} FromConstant c a => FromConstant c (UVar a) where
  fromConstant = ConstUVar . fromConstant

instance {-# OVERLAPPING #-} FromConstant (UVar a) (UVar a)

instance {-# OVERLAPPING #-} FromConstant c a => FromConstant (Partial c) (UVar a) where
  fromConstant (Known c) = ConstUVar (fromConstant c)
  fromConstant Unknown = More

instance (Scale k a, AdditiveMonoid k, Eq k, AdditiveMonoid a) => Scale k (UVar a) where
  scale k v
    | k == zero = ConstUVar zero
    | otherwise = fmap (scale k) v

instance {-# OVERLAPPING #-} (Semiring a, Eq a) => Scale (UVar a) (UVar a) where
  scale = (*)

instance (Exponent a e, MultiplicativeMonoid a, Integral e) => Exponent (UVar a) e where
  _ ^ 0 = ConstUVar one
  v ^ 1 = v
  ConstUVar c ^ n = ConstUVar (c ^ n)
  _ ^ _ = More

instance (AdditiveMonoid a, Eq a) => AdditiveSemigroup (UVar a) where
  ConstUVar c + x = c .+ x
  x + ConstUVar c = c .+ x
  LinUVar k1 x1 b1 + LinUVar k2 x2 b2
    | x1 == x2 =
        if k1 + k2 == zero
          then ConstUVar (b1 + b2)
          else LinUVar (k1 + k2) x1 (b1 + b2)
  _ + _ = More

(.+) :: AdditiveSemigroup a => a -> UVar a -> UVar a
c1 .+ ConstUVar c2 = ConstUVar (c1 + c2)
c .+ LinUVar k x b = LinUVar k x (b + c)
_ .+ More = More

instance Zero a => Zero (UVar a) where
  zero = ConstUVar zero

instance (AdditiveMonoid a, Eq a) => AdditiveMonoid (UVar a)

instance (AdditiveGroup a, Eq a) => AdditiveGroup (UVar a) where
  negate = fmap negate

instance (Semiring a, Eq a) => MultiplicativeSemigroup (UVar a) where
  ConstUVar c * x = scale c x
  x * ConstUVar c = scale c x
  _ * _ = More

instance (Semiring a, Eq a) => MultiplicativeMonoid (UVar a) where
  one = ConstUVar one

instance (Semiring a, Eq a) => Semiring (UVar a)

instance (Ring a, Eq a) => Ring (UVar a)

instance Eq a => ZkFold.Eq (UVar a) where
  type BooleanOf (UVar a) = Partial Bool
  ConstUVar c == ConstUVar d = Known (c == d)
  _ == _ = Unknown

instance (Field a, Eq a) => Field (UVar a) where
  finv (ConstUVar c) = ConstUVar (finv c)
  finv _ = More

instance Finite a => Finite (UVar a) where
  type Order (UVar a) = Order a

instance (PrimeField a, Eq a, BooleanOf a ~ Bool) => PrimeField (UVar a) where
  type IntegralOf (UVar a) = Partial (IntegralOf a)
  toIntegral (ConstUVar c) = Known (toIntegral c)
  toIntegral _ = Unknown

data Partial a = Known a | Unknown
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
    via (ApplicativeAlgebra Partial a)

deriving via
  (ApplicativeAlgebra Partial a)
  instance
    FromConstant c a => FromConstant c (Partial a)

deriving via
  (ApplicativeAlgebra Partial a)
  instance
    Scale k a => Scale k (Partial a)

deriving via
  (ApplicativeAlgebra Partial a)
  instance
    AdditiveMonoid a => AdditiveMonoid (Partial a)

instance Applicative Partial where
  pure = Known
  Known f <*> Known x = Known (f x)
  _ <*> _ = Unknown

instance Conditional b a => Conditional (Partial b) (Partial a) where
  bool = liftA3 bool

instance Conditional b a => Conditional (Partial b) (UVar a) where
  bool (ConstUVar x) (ConstUVar y) (Known b) = ConstUVar (bool x y b)
  bool _ _ _ = More

instance ZkFold.Eq a => ZkFold.Eq (Partial a) where
  type BooleanOf (Partial a) = Partial (BooleanOf a)
  (==) = liftA2 (ZkFold.==)

instance Ord a => Ord (Partial a) where
  type OrderingOf (Partial a) = Partial (OrderingOf a)
  compare = liftA2 compare

instance {-# OVERLAPPING #-} FromConstant (Partial a) (Partial a)

instance Exponent a e => Exponent (Partial a) e where
  x ^ p = fmap (^ p) x

instance {-# OVERLAPPING #-} MultiplicativeSemigroup a => Scale (Partial a) (Partial a)

instance Semiring a => Semiring (Partial a)

instance Ring a => Ring (Partial a)

instance SemiEuclidean a => SemiEuclidean (Partial a) where
  div = liftA2 div
  mod = liftA2 mod

instance Euclidean a => Euclidean (Partial a) where
  gcd = liftA2 gcd
  bezoutL = liftA2 bezoutL
  bezoutR = liftA2 bezoutR
