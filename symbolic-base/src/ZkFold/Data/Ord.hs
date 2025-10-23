{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Data.Ord where

import qualified Data.Bool as Haskell
import Data.Foldable (Foldable (fold))
import Data.Functor.Constant (Constant (..))
import Data.Monoid (Monoid)
import qualified Data.Ord as Haskell
import Data.Semialign (Semialign, alignWith)
import Data.Semigroup ((<>))
import qualified Data.String as Haskell
import Data.These (These (..))
import Data.Type.Equality (type (~))
import qualified GHC.Generics as G
import qualified GHC.Integer as Haskell
import qualified Numeric.Natural as Haskell

import ZkFold.Control.Conditional (Conditional, ifThenElse)
import ZkFold.Data.Eq

class (Monoid ordering, Eq ordering) => IsOrdering ordering where
  lt, eq, gt :: ordering

instance IsOrdering Haskell.Ordering where
  lt = Haskell.LT
  eq = Haskell.EQ
  gt = Haskell.GT

class
  ( Eq a
  , IsOrdering (OrderingOf a)
  , BooleanOf (OrderingOf a) ~ BooleanOf a
  ) =>
  Ord a
  where
  type OrderingOf a
  type OrderingOf a = GOrderingOf (G.Rep a)

  compare :: a -> a -> OrderingOf a
  default compare
    :: (G.Generic a, GOrd (G.Rep a), OrderingOf a ~ GOrderingOf (G.Rep a))
    => a -> a -> OrderingOf a
  compare x y = gcompare (G.from x) (G.from y)

(<), (<=), (>=), (>) :: Ord a => a -> a -> BooleanOf a

infix 4 <, <=, >=, >

x < y = compare x y == lt

x <= y = compare x y /= gt

x >= y = compare x y /= lt

x > y = compare x y == gt

max, min :: (Ord a, Conditional (BooleanOf a) a) => a -> a -> a
max x y = ifThenElse (x < y) y x
min x y = ifThenElse (x > y) y x

instance Haskell.Ord a => Ord (HaskellEqOrd a) where
  type OrderingOf (HaskellEqOrd a) = Haskell.Ordering
  compare = Haskell.compare

deriving via (HaskellEqOrd Haskell.Natural) instance Ord Haskell.Natural

deriving via (HaskellEqOrd Haskell.Integer) instance Ord Haskell.Integer

deriving via (HaskellEqOrd Haskell.Bool) instance Ord Haskell.Bool

deriving via (HaskellEqOrd Haskell.String) instance Ord Haskell.String

deriving newtype instance Ord a => Ord (Constant a b)

instance (Foldable f, Semialign f, Ord a) => Ord (SemialignEqOrd f a) where
  type OrderingOf (SemialignEqOrd f a) = OrderingOf a
  MkSemialignEqOrd f `compare` MkSemialignEqOrd g = fold (alignWith combine f g)
   where
    combine = \case
      This _ -> lt
      These x y -> compare x y
      That _ -> gt

class
  ( GEq u
  , IsOrdering (GOrderingOf u)
  , BooleanOf (GOrderingOf u) ~ GBooleanOf u
  ) =>
  GOrd u
  where
  type GOrderingOf u
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
  gcompare (x0 G.:*: x1) (y0 G.:*: y1) = gcompare x0 y0 <> gcompare x1 y1

instance GOrd v => GOrd (G.M1 i c v) where
  type GOrderingOf (G.M1 i c v) = GOrderingOf v
  gcompare (G.M1 x) (G.M1 y) = gcompare x y

instance Ord x => GOrd (G.Rec0 x) where
  type GOrderingOf (G.Rec0 x) = OrderingOf x
  gcompare (G.K1 x) (G.K1 y) = compare x y
