{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Eq (
    Eq(..),
    elem,
    SymbolicEq,
    GEq (..)
) where

import           Data.Bool                        (bool)
import           Data.Foldable                    (Foldable)
import           Data.Functor.Rep                 (mzipRep, mzipWithRep)
import           Data.Traversable                 (for)
import qualified Data.Vector                      as V
import qualified GHC.Generics                     as G
import           Prelude                          (return, type (~), ($))
import qualified Prelude                          as Haskell

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Field
import           ZkFold.Algebra.Number
import           ZkFold.Data.Package
import           ZkFold.Data.Vector
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool        (Bool (Bool), BoolType (..), all, any)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators (runInvert)
import           ZkFold.Symbolic.Data.Conditional (Conditional, GConditional)
import           ZkFold.Symbolic.MonadCircuit

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

instance Eq Natural where
    type BooleanOf Natural = Haskell.Bool
    (==) = (Haskell.==)
    (/=) = (Haskell./=)
instance Eq Haskell.Bool where
    type BooleanOf Haskell.Bool = Haskell.Bool
    (==) = (Haskell.==)
    (/=) = (Haskell./=)
instance Eq Haskell.String where
    type BooleanOf Haskell.String = Haskell.Bool
    (==) = (Haskell.==)
    (/=) = (Haskell./=)
instance KnownNat n => Eq (Zp n) where
    type BooleanOf (Zp n) = Haskell.Bool
    (==) = (Haskell.==)
    (/=) = (Haskell./=)

instance (Symbolic c, LayoutFunctor f)
  => Eq (c f) where
    type BooleanOf (c f) = Bool c
    x == y =
        let
            result = symbolic2F x y
                (mzipWithRep (\i j -> bool zero one (i Haskell.== j)))
                (\x' y' -> do
                    difference <- for (mzipRep x' y') $ \(i, j) ->
                        newAssigned (\w -> w i - w j)
                    (isZeros, _) <- runInvert difference
                    return isZeros
                )
        in
            all Bool (unpacked result)

    x /= y =
        let
            result = symbolic2F x y
                (mzipWithRep (\i j -> bool zero one (i Haskell./= j)))
                (\x' y' -> do
                    difference <- for (mzipRep x' y') $ \(i, j) ->
                        newAssigned (\w -> w i - w j)
                    (isZeros, _) <- runInvert difference
                    for isZeros $ \isZ ->
                      newAssigned (\w -> one - w isZ)
                )
        in
            any Bool (unpacked result)

type SymbolicEq x =
  ( SymbolicOutput x
  , Eq x
  , BooleanOf x ~ Bool (Context x)
  )

instance (KnownNat n, Eq x) => Eq (Vector n x) where
    type BooleanOf (Vector n x) = BooleanOf x
    u == v = V.foldl (&&) true (V.zipWith (==) (toV u) (toV v))
    u /= v = V.foldl (||) false (V.zipWith (/=) (toV u) (toV v))

deriving newtype instance Symbolic c => Eq (Bool c)

instance (Eq x0, Eq x1, BooleanOf x0 ~ BooleanOf x1) => Eq (x0,x1)
instance
  ( Eq x0, Eq x1, Eq x2
  , BooleanOf x0 ~ BooleanOf x1
  , BooleanOf x1 ~ BooleanOf x2
  ) => Eq (x0,x1,x2)
instance
  ( Eq x0, Eq x1, Eq x2, Eq x3
  , BooleanOf x0 ~ BooleanOf x1
  , BooleanOf x1 ~ BooleanOf x2
  , BooleanOf x2 ~ BooleanOf x3
  ) => Eq (x0,x1,x2,x3)

instance Eq field => Eq (Ext2 field i)
instance Eq field => Eq (Ext3 field i)

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
