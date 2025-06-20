{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Data.Eq where

import Data.Bool (Bool)
import qualified Data.Eq as Haskell
import Data.Foldable (Foldable)
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Data.Ratio (Rational)
import Data.String (String)
import Data.Type.Equality (type (~))
import qualified GHC.Generics as G
import Numeric.Natural (Natural)
import ZkFold.Control.Conditional (Conditional, GConditional)
import ZkFold.Data.Bool
import Prelude (Integer)

class Conditional (BooleanOf a) a => Eq a where
  type BooleanOf a
  type BooleanOf a = GBooleanOf (G.Rep a)

  infix 4 ==
  (==) :: a -> a -> BooleanOf a
  default (==)
    :: (G.Generic a, GEq (G.Rep a), BooleanOf a ~ GBooleanOf (G.Rep a))
    => a -> a -> BooleanOf a
  x == y = geq (G.from x) (G.from y)

  infix 4 /=
  (/=) :: a -> a -> BooleanOf a
  default (/=)
    :: (G.Generic a, GEq (G.Rep a), BooleanOf a ~ GBooleanOf (G.Rep a))
    => a -> a -> BooleanOf a
  x /= y = gneq (G.from x) (G.from y)

elem :: (Eq a, Foldable t) => a -> t a -> BooleanOf a
elem x = any (== x)

-- TODO: newtype for deriving of our Eq from Haskell.Eq

instance Eq Natural where
  type BooleanOf Natural = Bool
  (==) = (Haskell.==)
  (/=) = (Haskell./=)

instance Eq Integer where
  type BooleanOf Integer = Bool
  (==) = (Haskell.==)
  (/=) = (Haskell./=)

instance Eq Int where
  type BooleanOf Int = Bool
  (==) = (Haskell.==)
  (/=) = (Haskell./=)

instance Eq Bool where
  type BooleanOf Bool = Bool
  (==) = (Haskell.==)
  (/=) = (Haskell./=)

instance Eq String where
  type BooleanOf String = Bool
  (==) = (Haskell.==)
  (/=) = (Haskell./=)

instance Eq Rational where
  type BooleanOf Rational = Bool
  (==) = (Haskell.==)
  (/=) = (Haskell./=)

instance (Eq a, Conditional (BooleanOf a) (Maybe a)) => Eq (Maybe a) where
  type BooleanOf (Maybe a) = BooleanOf a
  Just x == Just y = x == y
  Nothing == Nothing = true
  _ == _ = false
  x /= y = not (x == y)

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

class GConditional (GBooleanOf u) u => GEq u where
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
