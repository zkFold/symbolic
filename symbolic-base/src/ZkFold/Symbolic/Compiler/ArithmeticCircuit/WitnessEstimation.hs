module ZkFold.Symbolic.Compiler.ArithmeticCircuit.WitnessEstimation where

import           Data.Bool                                      (otherwise)
import           Data.Eq                                        (Eq, (==))
import           Data.Function                                  ((.))
import           Data.Functor                                   (Functor, fmap)
import           Data.Maybe                                     (Maybe (..))
import           Prelude                                        (Integral)

import           ZkFold.Algebra.Class
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var (NewVar)
import           ZkFold.Symbolic.MonadCircuit                   (IntegralOf, ResidueField, fromIntegral, toIntegral)

data UVar a = ConstUVar a | LinUVar a NewVar a | More deriving Functor

instance FromConstant c a => FromConstant c (UVar a) where
    fromConstant = ConstUVar . fromConstant

instance {-# OVERLAPPING #-} FromConstant (UVar a) (UVar a)

instance (Scale k a, AdditiveMonoid k, Eq k, AdditiveMonoid a) => Scale k (UVar a) where
    scale k v
        | k == zero = ConstUVar zero
        | otherwise = fmap (scale k) v

instance {-# OVERLAPPING #-} (Semiring a, Eq a) => Scale (UVar a) (UVar a) where
    scale = (*)

instance (Exponent a e, MultiplicativeMonoid a, Integral e) => Exponent (UVar a) e where
    _ ^ 0           = ConstUVar one
    v ^ 1           = v
    ConstUVar c ^ n = ConstUVar (c ^ n)
    _ ^ _           = More

instance (AdditiveMonoid a, Eq a) => AdditiveSemigroup (UVar a) where
    ConstUVar c + x = c .+ x
    x + ConstUVar c = c .+ x
    LinUVar k1 x1 b1 + LinUVar k2 x2 b2
        | x1 == x2 = if k1 + k2 == zero
                     then ConstUVar (b1 + b2)
                     else LinUVar (k1 + k2) x1 (b1 + b2)
    _ + _ = More

(.+) :: AdditiveSemigroup a => a -> UVar a -> UVar a
c1 .+ ConstUVar c2 = ConstUVar (c1 + c2)
c .+ LinUVar k x b = LinUVar k x (b + c)
_ .+ More          = More

instance (AdditiveMonoid a, Eq a) => AdditiveMonoid (UVar a) where
    zero = ConstUVar zero

instance (AdditiveGroup a, Eq a) => AdditiveGroup (UVar a) where
    negate = fmap negate

instance (Semiring a, Eq a) => MultiplicativeSemigroup (UVar a) where
    ConstUVar c * x = scale c x
    x * ConstUVar c = scale c x
    _ * _           = More

instance (Semiring a, Eq a) => MultiplicativeMonoid (UVar a) where
    one = ConstUVar one

instance (Semiring a, Eq a) => Semiring (UVar a)

instance (Ring a, Eq a) => Ring (UVar a)

instance (Field a, Eq a) => Field (UVar a) where
    finv (ConstUVar c) = ConstUVar (finv c)
    finv _             = More

instance Finite a => Finite (UVar a) where
    type Order (UVar a) = Order a

instance (ResidueField a, Eq a) => ResidueField (UVar a) where
    type IntegralOf (UVar a) = Maybe (IntegralOf a)
    fromIntegral (Just x) = ConstUVar (fromIntegral x)
    fromIntegral Nothing  = More
    toIntegral (ConstUVar c) = Just (toIntegral c)
    toIntegral _             = Nothing
