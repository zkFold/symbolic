module ZkFold.Algebra.Polynomial.Multivariate.Maps (Polynomial, evalPoly, polyToList, traversePoly) where

import Control.Applicative (Applicative)
import Data.Bifunctor (first)
import Data.Bool (otherwise)
import Data.Eq (Eq, (/=))
import Data.Function (($), (.))
import Data.Functor (fmap)
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (Ord)
import Data.Semigroup ((<>))
import Data.Traversable (traverse)
import Data.Tuple (snd, swap)
import GHC.IsList (IsList (..))
import Numeric.Natural (Natural)

import ZkFold.Algebra.Class

newtype Polynomial a v = Polynomial {polynomial :: Map (Map v Natural) a}

evalPoly :: Algebra a b => Polynomial a v -> (v -> b) -> b
evalPoly (Polynomial p) x =
  M.foldlWithKey'
    (\b m c -> b + M.foldlWithKey' (\b' v e -> b' * x v ^ e) (fromConstant c) m)
    zero
    p

polyToList :: Polynomial a v -> [([(v, Natural)], a)]
polyToList = fmap (first M.toList) . M.toList . polynomial

instance (AdditiveMonoid a, Eq a, Ord v) => IsList (Polynomial a v) where
  type Item (Polynomial a v) = ([(v, Natural)], a)
  toList = polyToList
  fromList =
    Polynomial
      . M.filter (/= zero)
      . M.fromListWith (+)
      . fmap (first $ M.fromListWith (+) . L.filter ((/= zero) . snd))

traversePoly
  :: (Applicative f, AdditiveMonoid a, Eq a, Ord v)
  => (u -> f v) -> Polynomial a u -> f (Polynomial a v)
traversePoly f =
  fmap fromList
    . traverse
      ( fmap swap
          . traverse (traverse $ fmap swap . traverse f . swap)
          . swap
      )
    . polyToList

instance Zero (Polynomial a v) where
  zero = Polynomial M.empty

instance (Scale k a, Eq k, Zero k) => Scale k (Polynomial a v) where
  scale k
    | k /= zero = Polynomial . fmap (scale k) . polynomial
    | otherwise = zero

instance (AdditiveMonoid a, Eq a, Ord v) => AdditiveSemigroup (Polynomial a v) where
  (toList -> p) + (toList -> q) = fromList (p <> q)

instance (AdditiveMonoid a, Eq a, Ord v) => AdditiveMonoid (Polynomial a v)

instance (AdditiveGroup a, Eq a, Ord v) => AdditiveGroup (Polynomial a v) where
  negate = Polynomial . fmap negate . polynomial

instance (Semiring a, Eq a, Ord v) => MultiplicativeSemigroup (Polynomial a v) where
  (toList -> p) * (toList -> q) =
    fromList [(m <> n, c * d) | (m, c) <- p, (n, d) <- q]

instance (Semiring a, Eq a, Ord v) => MultiplicativeMonoid (Polynomial a v) where
  one = fromConstant (one :: a)

instance (FromConstant c a, Eq a, Zero a) => FromConstant c (Polynomial a v) where
  fromConstant (fromConstant -> (x :: a))
    | x /= zero = Polynomial (M.singleton M.empty x)
    | otherwise = zero

instance (Semiring a, Eq a, Ord v) => Exponent (Polynomial a v) Natural where
  (^) = natPow

instance {-# OVERLAPPING #-} FromConstant (Polynomial a v) (Polynomial a v)

instance {-# OVERLAPPING #-} (Semiring a, Eq a, Ord v) => Scale (Polynomial a v) (Polynomial a v)

instance (Semiring a, Eq a, Ord v) => Semiring (Polynomial a v)

instance (Ring a, Eq a, Ord v) => Ring (Polynomial a v)
