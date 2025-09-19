{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Data.Eq where

import Data.Bool (Bool)
import qualified Data.Eq as Haskell
import Data.Foldable (Foldable)
import Data.Functor.Constant (Constant)
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import qualified Data.Ord as Haskell
import Data.Ratio (Rational)
import Data.String (String)
import Data.Type.Equality (type (~))
import GHC.Generics ((:.:))
import qualified GHC.Generics as G
import Numeric.Natural (Natural)
import ZkFold.Data.Bool
import Prelude (Integer)

class BoolType (BooleanOf a) => Eq a where
  type BooleanOf a
  type BooleanOf a = GBooleanOf (G.Rep a)

  infix 4 ==
  (==) :: a -> a -> BooleanOf a
  default (==)
    :: (G.Generic a, GEq (G.Rep a), BooleanOf a ~ GBooleanOf (G.Rep a))
    => a
    -> a
    -> BooleanOf a
  x == y = geq (G.from x) (G.from y)

  infix 4 /=
  (/=) :: a -> a -> BooleanOf a
  default (/=)
    :: (G.Generic a, GEq (G.Rep a), BooleanOf a ~ GBooleanOf (G.Rep a))
    => a
    -> a
    -> BooleanOf a
  x /= y = gneq (G.from x) (G.from y)

elem :: (Eq a, Foldable t) => a -> t a -> BooleanOf a
elem x = any (== x)

newtype HaskellEqOrd a = HaskellEqOrd {runHaskellEqOrd :: a}
  deriving (Haskell.Eq, Haskell.Ord)

instance Haskell.Eq a => Eq (HaskellEqOrd a) where
  type BooleanOf (HaskellEqOrd a) = Bool
  (==) = (Haskell.==)
  (/=) = (Haskell./=)

deriving via (HaskellEqOrd Natural) instance Eq Natural

deriving via (HaskellEqOrd Integer) instance Eq Integer

deriving via (HaskellEqOrd Int) instance Eq Int

deriving via (HaskellEqOrd Bool) instance Eq Bool

deriving via (HaskellEqOrd String) instance Eq String

deriving via (HaskellEqOrd Rational) instance Eq Rational

instance Eq a => Eq (Maybe a) where
  type BooleanOf (Maybe a) = BooleanOf a
  Just x == Just y = x == y
  Nothing == Nothing = true
  _ == _ = false
  x /= y = not (x == y)

instance Eq a => Eq (Constant a b)

instance Eq (f a) => Eq (G.Rec1 f a)

instance Eq (f a) => Eq (G.M1 i c f a)

instance Eq (f (g a)) => Eq ((:.:) f g a)

instance {-# OVERLAPPING #-} Eq (f a) => Eq ((f G.:*: G.U1) a) where
  (x G.:*: _) == (y G.:*: _) = x == y
  (x G.:*: _) /= (y G.:*: _) = x /= y

instance (Eq (f a), Eq (g a), BooleanOf (f a) ~ BooleanOf (g a)) => Eq ((f G.:*: g) a)

instance (Eq x0, Eq x1, BooleanOf x0 ~ BooleanOf x1) => Eq (x0, x1)

instance
  ( Eq x0
  , Eq x1
  , Eq x2
  , BooleanOf x0 ~ BooleanOf x1
  , BooleanOf x1 ~ BooleanOf x2
  )
  => Eq (x0, x1, x2)

instance
  ( Eq x0
  , Eq x1
  , Eq x2
  , Eq x3
  , BooleanOf x0 ~ BooleanOf x1
  , BooleanOf x1 ~ BooleanOf x2
  , BooleanOf x2 ~ BooleanOf x3
  )
  => Eq (x0, x1, x2, x3)

class BoolType (GBooleanOf u) => GEq u where
  type GBooleanOf u
  geq :: u x -> u x -> GBooleanOf u
  gneq :: u x -> u x -> GBooleanOf u

instance (GBooleanOf u ~ GBooleanOf v, GEq u, GEq v) => GEq (u G.:*: v) where
  type GBooleanOf (u G.:*: v) = GBooleanOf u
  geq (x0 G.:*: x1) (y0 G.:*: y1) = geq x0 y0 && geq x1 y1
  gneq (x0 G.:*: x1) (y0 G.:*: y1) = gneq x0 y0 || gneq x1 y1

instance GEq v => GEq (G.M1 i c v) where
  type GBooleanOf (G.M1 i c v) = GBooleanOf v
  geq (G.M1 x) (G.M1 y) = geq x y
  gneq (G.M1 x) (G.M1 y) = gneq x y

instance Eq x => GEq (G.Rec0 x) where
  type GBooleanOf (G.Rec0 x) = BooleanOf x
  geq (G.K1 x) (G.K1 y) = x == y
  gneq (G.K1 x) (G.K1 y) = x /= y
