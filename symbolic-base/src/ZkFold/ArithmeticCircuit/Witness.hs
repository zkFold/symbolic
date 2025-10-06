module ZkFold.ArithmeticCircuit.Witness where

import Control.Applicative (Applicative (..))
import Control.DeepSeq (NFData (..), rwhnf)
import Control.Monad (Monad (..), ap)
import Data.Function (const, (.))
import Data.Functor (Functor)
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import GHC.Integer (Integer)
import Numeric.Natural (Natural)

import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Bool (BoolType (..))
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.Ord (IsOrdering (..), Ord (..))
import Data.Bool (Bool)
import GHC.Real (odd)
import Data.Ord (Ordering (..))

type IsWitness a w = (Scale a w, FromConstant a w, PrimeField w)

newtype WitnessF a v = WitnessF {runWitnessF :: forall w. IsWitness a w => (v -> w) -> w}
  deriving Functor

instance NFData (WitnessF a v) where
  -- From instance NFData (a -> b):
  -- This instance is for convenience and consistency with seq.
  -- This assumes that WHNF is equivalent to NF for functions.
  rnf = rwhnf

instance Applicative (WitnessF a) where
  pure v = WitnessF (\x -> x v)
  (<*>) = ap

instance Monad (WitnessF a) where
  WitnessF f >>= k = WitnessF (\x -> f (\a -> runWitnessF (k a) x))

instance FromConstant Natural (WitnessF a v) where fromConstant x = WitnessF (fromConstant x)

instance FromConstant Integer (WitnessF a v) where fromConstant x = WitnessF (fromConstant x)

instance {-# INCOHERENT #-} FromConstant a (WitnessF a v) where fromConstant x = WitnessF (fromConstant x)

instance FromConstant (EuclideanF a v) (WitnessF a v) where
  fromConstant (EuclideanF f) = WitnessF (fromConstant . f)

instance Scale Natural (WitnessF a v) where scale k (WitnessF f) = WitnessF (scale k f)

instance Scale Integer (WitnessF a v) where scale k (WitnessF f) = WitnessF (scale k f)

instance {-# INCOHERENT #-} Scale a (WitnessF a v) where scale k (WitnessF f) = WitnessF (scale k f)

instance Exponent (WitnessF a v) Natural where WitnessF f ^ p = WitnessF (f ^ p)

instance Exponent (WitnessF a v) Integer where WitnessF f ^ p = WitnessF (f ^ p)

instance AdditiveSemigroup (WitnessF a v) where WitnessF f + WitnessF g = WitnessF (f + g)

instance Zero (WitnessF a v) where zero = WitnessF zero

instance AdditiveMonoid (WitnessF a v)

instance AdditiveGroup (WitnessF a v) where
  negate (WitnessF f) = WitnessF (negate f)
  WitnessF f - WitnessF g = WitnessF (f - g)

instance MultiplicativeSemigroup (WitnessF a v) where WitnessF f * WitnessF g = WitnessF (f * g)

instance MultiplicativeMonoid (WitnessF a v) where one = WitnessF one

instance Semiring (WitnessF a v)

instance Ring (WitnessF a v)

instance Conditional (BooleanF a v) (WitnessF a v) where
  bool (WitnessF f) (WitnessF g) (BooleanF b) = WitnessF (\x -> bool (f x) (g x) (b x))

instance Eq (WitnessF a v) where
  type BooleanOf (WitnessF a v) = BooleanF a v
  WitnessF f == WitnessF g = BooleanF (\x -> f x == g x)
  WitnessF f /= WitnessF g = BooleanF (\x -> f x /= g x)

instance Field (WitnessF a v) where
  finv (WitnessF f) = WitnessF (finv . f)
  WitnessF f // WitnessF g = WitnessF (\x -> f x // g x)

instance Finite a => Finite (WitnessF a v) where type Order (WitnessF a v) = Order a

instance PrimeField a => PrimeField (WitnessF a v) where
  type IntegralOf (WitnessF a v) = EuclideanF a v
  toIntegral (WitnessF f) = EuclideanF (toIntegral . f)

newtype BooleanF a v = BooleanF {booleanF :: forall w. IsWitness a w => (v -> w) -> BooleanOf w}

instance Conditional (BooleanF a v) (BooleanF a v) where
  bool (BooleanF f) (BooleanF g) (BooleanF h) =
    BooleanF (\x -> bool (f x) (g x) (h x))

instance BoolType (BooleanF a v) where
  true = BooleanF (const true)
  false = BooleanF (const false)
  not (BooleanF f) = BooleanF (not . f)
  BooleanF f && BooleanF g = BooleanF (\x -> f x && g x)
  BooleanF f || BooleanF g = BooleanF (\x -> f x || g x)
  BooleanF f `xor` BooleanF g = BooleanF (\x -> f x `xor` g x)

instance FromConstant Bool (BooleanF a v) where fromConstant = bool false true

instance FromConstant Integer (BooleanF a v) where fromConstant x = fromConstant (odd x)

newtype EuclideanF a v = EuclideanF {euclideanF :: forall w. IsWitness a w => (v -> w) -> IntegralOf w}

instance FromConstant Natural (EuclideanF a v) where fromConstant x = EuclideanF (fromConstant x)

instance FromConstant Integer (EuclideanF a v) where fromConstant x = EuclideanF (fromConstant x)

instance Conditional (BooleanF a v) (EuclideanF a v) where
  bool (EuclideanF f) (EuclideanF g) (BooleanF h) =
    EuclideanF (\x -> bool (f x) (g x) (h x))

instance Eq (EuclideanF a v) where
  type BooleanOf (EuclideanF a v) = BooleanF a v
  EuclideanF f == EuclideanF g = BooleanF (\x -> f x == g x)
  EuclideanF f /= EuclideanF g = BooleanF (\x -> f x /= g x)

instance Ord (EuclideanF a v) where
  type OrderingOf (EuclideanF a v) = OrderingF a v
  ordering (EuclideanF f) (EuclideanF g) (EuclideanF h) (OrderingF o) =
    EuclideanF (\x -> ordering (f x) (g x) (h x) (o x))
  compare (EuclideanF f) (EuclideanF g) = OrderingF (\x -> compare (f x) (g x))
  EuclideanF f < EuclideanF g = BooleanF (\x -> f x < g x)
  EuclideanF f <= EuclideanF g = BooleanF (\x -> f x <= g x)
  EuclideanF f >= EuclideanF g = BooleanF (\x -> f x >= g x)
  EuclideanF f > EuclideanF g = BooleanF (\x -> f x > g x)

instance Scale Natural (EuclideanF a v) where scale k (EuclideanF f) = EuclideanF (scale k f)

instance Scale Integer (EuclideanF a v) where scale k (EuclideanF f) = EuclideanF (scale k f)

instance Exponent (EuclideanF a v) Natural where EuclideanF f ^ p = EuclideanF (f ^ p)

instance AdditiveSemigroup (EuclideanF a v) where EuclideanF f + EuclideanF g = EuclideanF (f + g)

instance Zero (EuclideanF a v) where zero = EuclideanF zero

instance AdditiveMonoid (EuclideanF a v)

instance AdditiveGroup (EuclideanF a v) where
  negate (EuclideanF f) = EuclideanF (negate f)
  EuclideanF f - EuclideanF g = EuclideanF (f - g)

instance MultiplicativeSemigroup (EuclideanF a v) where EuclideanF f * EuclideanF g = EuclideanF (f * g)

instance MultiplicativeMonoid (EuclideanF a v) where one = EuclideanF one

instance Semiring (EuclideanF a v)

instance Ring (EuclideanF a v)

instance SemiEuclidean (EuclideanF a v) where
  EuclideanF f `div` EuclideanF g = EuclideanF (\x -> f x `div` g x)
  EuclideanF f `mod` EuclideanF g = EuclideanF (\x -> f x `mod` g x)

instance Euclidean (EuclideanF a v) where
  EuclideanF f `gcd` EuclideanF g = EuclideanF (\x -> f x `gcd` g x)
  EuclideanF f `bezoutL` EuclideanF g = EuclideanF (\x -> f x `bezoutL` g x)
  EuclideanF f `bezoutR` EuclideanF g = EuclideanF (\x -> f x `bezoutR` g x)

newtype OrderingF a v = OrderingF
  {orderingF :: forall w. IsWitness a w => (v -> w) -> OrderingOf (IntegralOf w)}

instance Semigroup (OrderingF a v) where
  OrderingF f <> OrderingF g = OrderingF (\x -> f x <> g x)

instance Monoid (OrderingF a v) where
  mempty = OrderingF (const mempty)

instance IsOrdering (OrderingF a v) where
  lt = OrderingF (const lt)
  eq = OrderingF (const eq)
  gt = OrderingF (const gt)

instance FromConstant Ordering (OrderingF a v) where
  fromConstant = \case
    LT -> lt
    EQ -> eq
    GT -> gt

intToOrdering :: Integer -> Ordering
intToOrdering x = case x `mod` 3 of
    0 -> EQ
    1 -> GT
    _ -> LT

instance FromConstant Integer (OrderingF a v) where
  fromConstant = fromConstant . intToOrdering
