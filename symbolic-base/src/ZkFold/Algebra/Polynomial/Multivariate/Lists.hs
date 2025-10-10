{-# LANGUAGE BlockArguments #-}

module ZkFold.Algebra.Polynomial.Multivariate.Lists (Polynomial, evalPoly) where

import Control.Applicative (Applicative (..))
import Data.Bool (otherwise)
import Data.Eq (Eq, (/=))
import Data.Function ((.), ($))
import Data.Functor (fmap, Functor)
import Data.Semigroup (Semigroup (..))
import Data.Traversable (traverse, Traversable)
import Numeric.Natural (Natural)

import ZkFold.Algebra.Class
import Data.Monoid (Monoid (..))
import Data.Foldable (Foldable (foldr))
import Data.Bifunctor (first)

newtype Bag a = Bag (forall c. (a -> c -> c) -> c -> c)
  deriving Functor

cons :: a -> Bag a -> Bag a
cons x (Bag f) = Bag \g s -> g x (f g s)

instance Semigroup (Bag a) where
  Bag f <> Bag g = Bag \h c -> f h (g h c)

instance Monoid (Bag a) where
  mempty = Bag \_ c -> c

instance Foldable Bag where
  foldr f s (Bag g) = g f s

instance Traversable Bag where
  traverse f (Bag g) = g (liftA2 cons . f) (pure mempty)

instance Applicative Bag where
  pure x = Bag \g s -> g x s
  Bag fs <*> Bag xs = Bag \h -> fs \f -> xs (h . f)

newtype Polynomial a v = Polynomial {polynomial :: Bag (a, Bag (v, Natural))}
  deriving (Functor, Foldable, Traversable)

evalPoly :: Algebra a b => Polynomial a v -> (v -> b) -> b
evalPoly (Polynomial p) x =
  foldr
    (\(k, b) s -> foldr (\(i, e) q -> x i ^ e * q) (fromConstant k) b + s)
    zero
    p

instance (Ring a, Eq a) => Applicative (Polynomial a) where
  pure = Polynomial . pure . (one,) . pure . (, one)
  fs <*> xs = evalPoly fs (`fmap` xs)

instance Zero (Polynomial a v) where
  zero = Polynomial mempty

instance (Scale k a, Eq k, Zero k) => Scale k (Polynomial a v) where
  scale k
    | k /= zero = Polynomial . fmap (first $ scale k) . polynomial
    | otherwise = zero

instance AdditiveSemigroup (Polynomial a v) where
  Polynomial p + Polynomial q = Polynomial (p <> q)

instance Scale Natural a => AdditiveMonoid (Polynomial a v)

instance AdditiveGroup a => AdditiveGroup (Polynomial a v) where
  negate = Polynomial . fmap (first negate) . polynomial

instance MultiplicativeSemigroup a => MultiplicativeSemigroup (Polynomial a v) where
  Polynomial p * Polynomial q =
    Polynomial $ liftA2 (\(c, m) (d, n) -> (c * d, m <> n)) p q

instance (Semiring a, Eq a) => MultiplicativeMonoid (Polynomial a v) where
  one = fromConstant (one :: a)

instance (FromConstant c a, Eq a, Zero a) => FromConstant c (Polynomial a v) where
  fromConstant (fromConstant -> (x :: a))
    | x /= zero = Polynomial $ pure (x, mempty)
    | otherwise = zero

instance (Semiring a, Eq a) => Exponent (Polynomial a v) Natural where
  (^) = natPow

instance {-# OVERLAPPING #-} FromConstant (Polynomial a v) (Polynomial a v)

instance {-# OVERLAPPING #-} MultiplicativeSemigroup a => Scale (Polynomial a v) (Polynomial a v)

instance (Semiring a, Eq a) => Semiring (Polynomial a v)

instance (Ring a, Eq a) => Ring (Polynomial a v)
