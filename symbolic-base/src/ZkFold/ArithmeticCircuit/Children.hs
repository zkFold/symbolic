{-# LANGUAGE DerivingStrategies #-}

module ZkFold.ArithmeticCircuit.Children where

import ZkFold.ArithmeticCircuit.Witness (WitnessF (..))
import Data.Set (Set)
import Data.Function ((.), flip, const, id)
import Data.Monoid (mempty, Monoid)
import Data.Semigroup (Semigroup, (<>))
import Data.Ord (Ord)
import ZkFold.Symbolic.MonadCircuit (ResidueField (..))
import ZkFold.Algebra.Class
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.Bool (BoolType (..))
import ZkFold.Control.Conditional (Conditional (..))

children :: forall a v. (Finite a, Ord v) => WitnessF a v -> Set v
children = runC @a . flip runWitnessF mempty

newtype Children a v = C { runC :: Set v }
  deriving newtype (Semigroup, Monoid)

instance {-# OVERLAPPABLE #-} Ord v => FromConstant c (Children a v) where
  fromConstant = const mempty

instance {-# OVERLAPPING #-} FromConstant (Children a v) (Children a v)

instance {-# OVERLAPPABLE #-} Scale c (Children a v) where
  scale = const id

instance {-# OVERLAPPING #-} Ord v => Scale (Children a v) (Children a v)

instance Exponent (Children a v) b where
  (^) = const

instance Ord v => BoolType (Children a v) where
  true = mempty
  false = mempty
  not = id
  (&&) = (<>)
  (||) = (<>)
  xor = (<>)

instance Ord v => Conditional (Children a v) (Children a v) where
  bool x y b = x <> y <> b

instance Ord v => Eq (Children a v) where
  type BooleanOf (Children a v) = Children a v
  (==) = (<>)
  (/=) = (<>)

instance Finite a => Finite (Children a v) where
  type Order (Children a v) = Order a

instance Ord v => AdditiveSemigroup (Children a v) where
  (+) = (<>)

instance Ord v => AdditiveMonoid (Children a v) where
  zero = mempty

instance Ord v => AdditiveGroup (Children a v) where
  negate = id

instance Ord v => MultiplicativeSemigroup (Children a v) where
  (*) = (<>)

instance Ord v => MultiplicativeMonoid (Children a v) where
  one = mempty

instance Ord v => Semiring (Children a v)

instance Ord v => Ring (Children a v)

instance Ord v => SemiEuclidean (Children a v) where
  div = (<>)
  mod = (<>)

instance Ord v => Euclidean (Children a v) where
  gcd = (<>)
  bezoutL = (<>)
  bezoutR = (<>)

instance Ord v => Field (Children a v) where
  finv = id

instance (Ord v, Finite a) => ResidueField (Children a v) where
  type IntegralOf (Children a v) = Children a v
  fromIntegral = id
  toIntegral = id
