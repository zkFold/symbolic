{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}

module ZkFold.Data.Ord where

import Data.Monoid (Monoid)
import qualified Data.Ord as Haskell
import ZkFold.Data.Eq
import qualified GHC.Generics as G
import Data.Type.Equality (type (~))
import ZkFold.Data.Bool
import Data.Semigroup ((<>))
import qualified Numeric.Natural as Haskell
import qualified Data.Bool as Haskell
import qualified Data.String as Haskell
import qualified GHC.Integer as Haskell
import Data.Functor.Constant (Constant (..))

class Monoid ordering => IsOrdering ordering where
  lt, eq, gt :: ordering

instance IsOrdering Haskell.Ordering where
  lt = Haskell.LT
  eq = Haskell.EQ
  gt = Haskell.GT

class (Eq a, IsOrdering (OrderingOf a)) => Ord a where
  type OrderingOf a
  type OrderingOf a = GOrderingOf (G.Rep a)

  ordering :: a -> a -> a -> OrderingOf a -> a
  default ordering
    :: (G.Generic a, GOrd (G.Rep a), OrderingOf a ~ GOrderingOf (G.Rep a))
    => a -> a -> a -> OrderingOf a -> a
  ordering ltCase eqCase gtCase o =
    G.to (gordering (G.from ltCase) (G.from eqCase) (G.from gtCase) o)

  compare :: a -> a -> OrderingOf a
  default compare
    :: (G.Generic a, GOrd (G.Rep a), OrderingOf a ~ GOrderingOf (G.Rep a))
    => a -> a -> OrderingOf a
  compare x y = gcompare (G.from x) (G.from y)

  (<), (<=), (>), (>=) :: a -> a -> BooleanOf a
  infix 4 <, <=, >, >=
  default (<)
    :: (Ord (BooleanOf a), OrderingOf (BooleanOf a) ~ OrderingOf a)
    => a -> a -> BooleanOf a
  x < y = ordering true false false (compare x y)
  default (<=)
    :: (Ord (BooleanOf a), OrderingOf (BooleanOf a) ~ OrderingOf a)
    => a -> a -> BooleanOf a
  x <= y = ordering true true false (compare x y)
  default (>)
    :: (Ord (BooleanOf a), OrderingOf (BooleanOf a) ~ OrderingOf a)
    => a -> a -> BooleanOf a
  x > y = ordering false false true (compare x y)
  default (>=)
    :: (Ord (BooleanOf a), OrderingOf (BooleanOf a) ~ OrderingOf a)
    => a -> a -> BooleanOf a
  x >= y = ordering false true true (compare x y)

  max, min :: a -> a -> a
  max x y = ordering y x x (compare x y)
  min x y = ordering x x y (compare x y)

instance Haskell.Ord a => Ord (HaskellEqOrd a) where
  type OrderingOf (HaskellEqOrd a) = Haskell.Ordering
  ordering x y z = \case
    Haskell.LT -> x
    Haskell.EQ -> y
    Haskell.GT -> z
  compare = Haskell.compare
  (>) = (Haskell.>)
  (>=) = (Haskell.>=)
  (<) = (Haskell.<)
  (<=) = (Haskell.<=)
  min = Haskell.min
  max = Haskell.max

deriving via (HaskellEqOrd Haskell.Natural) instance Ord Haskell.Natural

deriving via (HaskellEqOrd Haskell.Integer) instance Ord Haskell.Integer

deriving via (HaskellEqOrd Haskell.Bool) instance Ord Haskell.Bool

deriving via (HaskellEqOrd Haskell.String) instance Ord Haskell.String

deriving newtype instance Ord a => Ord (Constant a b)

class (GEq u, IsOrdering (GOrderingOf u)) => GOrd u where
  type GOrderingOf u
  gordering :: u x -> u x -> u x -> GOrderingOf u -> u x
  gcompare :: u x -> u x -> GOrderingOf u

instance
  ( GOrd u
  , GOrd v
  , GBooleanOf u ~ GBooleanOf v
  , GOrderingOf u ~ GOrderingOf v
  )
  => GOrd (u G.:*: v)
  where
  type GOrderingOf (u G.:*: v) = GOrderingOf u
  gordering (lt0 G.:*: lt1) (eq0 G.:*: eq1) (gt0 G.:*: gt1) o =
    gordering lt0 eq0 gt0 o G.:*: gordering lt1 eq1 gt1 o
  gcompare (x0 G.:*: x1) (y0 G.:*: y1) = gcompare x0 y0 <> gcompare x1 y1

instance GOrd v => GOrd (G.M1 i c v) where
  type GOrderingOf (G.M1 i c v) = GOrderingOf v
  gordering (G.M1 ltCase) (G.M1 eqCase) (G.M1 gtCase) o =
    G.M1 (gordering ltCase eqCase gtCase o)
  gcompare (G.M1 x) (G.M1 y) = gcompare x y

instance Ord x => GOrd (G.Rec0 x) where
  type GOrderingOf (G.Rec0 x) = OrderingOf x
  gordering (G.K1 ltCase) (G.K1 eqCase) (G.K1 gtCase) o =
    G.K1 (ordering ltCase eqCase gtCase o)
  gcompare (G.K1 x) (G.K1 y) = compare x y
