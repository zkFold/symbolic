{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.ArithmeticCircuit.Witness where

import Control.Applicative (Applicative (..))
import Control.DeepSeq (NFData (..), rwhnf)
import Control.Monad (Monad (..), ap)
import Data.Function (const, (.))
import Data.Functor (Functor)
import GHC.Generics (Generic (..), K1 (..), M1 (..), (:*:) (..))
import Numeric.Natural (Natural)
import Prelude (Integer)

import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Bool (BoolType (..))
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Symbolic.MonadCircuit (ResidueField (..))

type IsWitness a w = (Scale a w, FromConstant a w, ResidueField w)

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

instance Scale Natural (WitnessF a v) where scale k (WitnessF f) = WitnessF (scale k f)

instance Scale Integer (WitnessF a v) where scale k (WitnessF f) = WitnessF (scale k f)

instance {-# INCOHERENT #-} Scale a (WitnessF a v) where scale k (WitnessF f) = WitnessF (scale k f)

instance Exponent (WitnessF a v) Natural where WitnessF f ^ p = WitnessF (f ^ p)

instance Exponent (WitnessF a v) Integer where WitnessF f ^ p = WitnessF (f ^ p)

instance AdditiveSemigroup (WitnessF a v) where WitnessF f + WitnessF g = WitnessF (f + g)

instance AdditiveMonoid (WitnessF a v) where zero = WitnessF zero

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

instance Finite a => ResidueField (WitnessF a v) where
  type IntegralOf (WitnessF a v) = EuclideanF a v
  fromIntegral (EuclideanF f) = WitnessF (fromIntegral . f)
  toIntegral (WitnessF f) = EuclideanF (toIntegral . f)

newtype BooleanF a v = BooleanF {booleanF :: forall w. IsWitness a w => (v -> w) -> BooleanOf w}

instance BoolType (BooleanF a v) where
  true = BooleanF (const true)
  false = BooleanF (const false)
  not (BooleanF f) = BooleanF (not . f)
  BooleanF f && BooleanF g = BooleanF (\x -> f x && g x)
  BooleanF f || BooleanF g = BooleanF (\x -> f x || g x)
  BooleanF f `xor` BooleanF g = BooleanF (\x -> f x `xor` g x)

newtype EuclideanF a v = EuclideanF {euclideanF :: forall w. IsWitness a w => (v -> w) -> IntegralOf w}

instance FromConstant Natural (EuclideanF a v) where fromConstant x = EuclideanF (fromConstant x)

instance FromConstant Integer (EuclideanF a v) where fromConstant x = EuclideanF (fromConstant x)

instance {-# OVERLAPPING #-} Conditional (BooleanE a v) (BooleanE a v) where
  bool (BooleanE f) (BooleanE g) (BooleanE b) = BooleanE (\x -> bool (f x) (g x) (b x))

instance {-# OVERLAPPING #-} Conditional (BooleanE a v) (EuclideanF a v) where
  bool (EuclideanF f) (EuclideanF g) (BooleanE b) = EuclideanF (\x -> bool (f x) (g x) (b x))

instance (Generic x, GConditional (BooleanE a v) (Rep x)) => Conditional (BooleanE a v) x where
  bool f g b = to (gbool (from f) (from g) b)

instance Eq (EuclideanF a v) where
  type BooleanOf (EuclideanF a v) = BooleanE a v
  EuclideanF f == EuclideanF g = BooleanE (\x -> f x == g x)
  EuclideanF f /= EuclideanF g = BooleanE (\x -> f x /= g x)

instance Scale Natural (EuclideanF a v) where scale k (EuclideanF f) = EuclideanF (scale k f)

instance Scale Integer (EuclideanF a v) where scale k (EuclideanF f) = EuclideanF (scale k f)

instance Exponent (EuclideanF a v) Natural where EuclideanF f ^ p = EuclideanF (f ^ p)

instance AdditiveSemigroup (EuclideanF a v) where EuclideanF f + EuclideanF g = EuclideanF (f + g)

instance AdditiveMonoid (EuclideanF a v) where zero = EuclideanF zero

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

newtype BooleanE a v = BooleanE {booleanE :: forall w. IsWitness a w => (v -> w) -> BooleanOf (IntegralOf w)}

instance BoolType (BooleanE a v) where
  true = BooleanE (const true)
  false = BooleanE (const false)
  not (BooleanE f) = BooleanE (not . f)
  BooleanE f && BooleanE g = BooleanE (\x -> f x && g x)
  BooleanE f || BooleanE g = BooleanE (\x -> f x || g x)
  BooleanE f `xor` BooleanE g = BooleanE (\x -> f x `xor` g x)

class GConditional b f where
  gbool :: f x -> f x -> b -> f x

instance Conditional (BooleanE a v) x => GConditional (BooleanE a v) (K1 i x) where
  gbool (K1 f) (K1 g) b = K1 (bool f g b)

instance GConditional b f => GConditional b (M1 i c f) where
  gbool (M1 f) (M1 g) b = M1 (gbool f g b)

instance (GConditional b f, GConditional b g) => GConditional b (f :*: g) where
  gbool (f1 :*: f2) (g1 :*: g2) b =
    gbool f1 g1 b :*: gbool f2 g2 b
