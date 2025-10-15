{-# LANGUAGE BlockArguments #-}

module ZkFold.Algebra.Polynomial.Multivariate.Expression (Polynomial, evalPoly) where

import Control.Applicative (Applicative (..))
import Data.Foldable (Foldable)
import Data.Function ((.))
import Data.Functor (Functor, (<$>))
import Data.Traversable (Traversable)
import Numeric.Natural (Natural)

import ZkFold.Algebra.Class

data Polynomial a v
  = PVar v | PConst a
  | Polynomial a v :+ Polynomial a v
  | Polynomial a v :* Polynomial a v
  deriving (Functor, Foldable, Traversable)

evalPoly :: Algebra a b => Polynomial a v -> (v -> b) -> b
evalPoly (PVar i) x = x i
evalPoly (PConst c) _ = fromConstant c
evalPoly (p :+ q) x = evalPoly p x + evalPoly q x
evalPoly (p :* q) x = evalPoly p x * evalPoly q x

instance Ring a => Applicative (Polynomial a) where
  pure = PVar
  fs <*> xs = evalPoly fs (<$> xs)

instance Zero a => Zero (Polynomial a v) where
  zero = PConst zero

instance FromConstant c a => Scale c (Polynomial a v)

instance AdditiveSemigroup (Polynomial a v) where
  (+) = (:+)

instance (FromConstant Natural a, Zero a) => AdditiveMonoid (Polynomial a v)

instance Ring a => AdditiveGroup (Polynomial a v) where
  negate = scale (negate one :: a)

instance MultiplicativeSemigroup (Polynomial a v) where
  (*) = (:*)

instance MultiplicativeMonoid a => MultiplicativeMonoid (Polynomial a v) where
  one = fromConstant (one :: a)

instance FromConstant c a => FromConstant c (Polynomial a v) where
  fromConstant = PConst . fromConstant

instance MultiplicativeMonoid a => Exponent (Polynomial a v) Natural where
  (^) = natPow

instance {-# OVERLAPPING #-} FromConstant (Polynomial a v) (Polynomial a v)

instance {-# OVERLAPPING #-} Scale (Polynomial a v) (Polynomial a v)

instance Semiring a => Semiring (Polynomial a v)

instance Ring a => Ring (Polynomial a v)
