{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Compiler.ArithmeticCircuit.WitnessEstimation where

import Control.Applicative (liftA2)
import Data.Bool (Bool (..), otherwise)
import qualified Data.Bool as Haskell
import Data.Eq (Eq, (==))
import Data.Function ((.))
import Data.Functor (Functor, fmap)
import Data.Maybe (Maybe (..))
import Prelude (Integral)

import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Bool
import qualified ZkFold.Data.Eq as ZkFold
import ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var (NewVar)
import ZkFold.Symbolic.MonadCircuit (IntegralOf, ResidueField, fromIntegral, toIntegral)

data UVar a = ConstUVar a | LinUVar a NewVar a | More deriving Functor

instance FromConstant c a => FromConstant c (UVar a) where
  fromConstant = ConstUVar . fromConstant

instance {-# OVERLAPPING #-} FromConstant (UVar a) (UVar a)

instance (AdditiveMonoid a, AdditiveMonoid k, Eq k, Scale k a) => Scale k (UVar a) where
  scale k v
    | k == zero = ConstUVar zero
    | otherwise = fmap (scale k) v

instance {-# OVERLAPPING #-} (Eq a, Semiring a) => Scale (UVar a) (UVar a) where
  scale = (*)

instance (Exponent a e, Integral e, MultiplicativeMonoid a) => Exponent (UVar a) e where
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

instance (AdditiveMonoid a, Eq a) => AdditiveMonoid (UVar a) where
  zero = ConstUVar zero

instance (AdditiveGroup a, Eq a) => AdditiveGroup (UVar a) where
  negate = fmap negate

instance (Eq a, Semiring a) => MultiplicativeSemigroup (UVar a) where
  ConstUVar c * x = scale c x
  x * ConstUVar c = scale c x
  _ * _ = More

instance (Eq a, Semiring a) => MultiplicativeMonoid (UVar a) where
  one = ConstUVar one

instance (Eq a, Semiring a) => Semiring (UVar a)

instance (Eq a, Ring a) => Ring (UVar a)

instance BoolType (Maybe Bool) where
  true = Just True
  false = Just False
  not = fmap not
  Just False && _ = false
  _ && Just False = false
  x && y = liftA2 (&&) x y
  Just True || _ = true
  _ || Just True = true
  x || y = liftA2 (||) x y
  xor = liftA2 xor

instance Conditional (Maybe Bool) (UVar a) where
  bool x y (Just b) = Haskell.bool x y b
  bool _ _ Nothing = More

instance Eq a => ZkFold.Eq (UVar a) where
  type BooleanOf (UVar a) = Maybe Bool
  ConstUVar c == ConstUVar d = Just (c == d)
  _ == _ = Nothing
  u /= v = not (u ZkFold.== v)

instance (Eq a, Field a) => Field (UVar a) where
  finv (ConstUVar c) = ConstUVar (finv c)
  finv _ = More

instance Finite a => Finite (UVar a) where
  type Order (UVar a) = Order a

instance (Eq a, ResidueField a) => ResidueField (UVar a) where
  type IntegralOf (UVar a) = Maybe (IntegralOf a)
  fromIntegral (Just x) = ConstUVar (fromIntegral x)
  fromIntegral Nothing = More
  toIntegral (ConstUVar c) = Just (toIntegral c)
  toIntegral _ = Nothing
