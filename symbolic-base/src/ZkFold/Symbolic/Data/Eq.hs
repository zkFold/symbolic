{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.Eq (
    Eq(..),
    elem,
    SymbolicEq,
    GEq (..)
) where

import           Data.Bool                        (bool)
import           Data.Functor.Rep                 (mzipRep, mzipWithRep)
import           Data.Traversable                 (for)
import           Prelude                          (return, type (~), ($))
import qualified Prelude                          as Haskell

import           ZkFold.Algebra.Class
import           ZkFold.Data.Eq
import           ZkFold.Data.Package
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool        (Bool (Bool), all, any)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators (runInvert)
import           ZkFold.Symbolic.MonadCircuit

-- TODO: move to ZkFold.Symbolic.Data.Bool

instance (Symbolic c, LayoutFunctor f) => Eq (c f) where
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

deriving newtype instance Symbolic c => Eq (Bool c)
