{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.Eq (
    Eq(..),
    elem,
    GEq (..)
) where

import           Data.Bool                        (bool)
import           Data.Functor.Rep                 (mzipRep, mzipWithRep)
import           Data.Traversable                 (for)
import qualified Prelude                          as Haskell

import           ZkFold.Algebra.Class
import           ZkFold.Data.Eq
import           ZkFold.Data.Package
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool        (Bool (..), all, any)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators (runInvert)
import           ZkFold.Symbolic.Data.Conditional ()
import           ZkFold.Symbolic.MonadCircuit
import Control.Monad (return)
import Data.Function (($))

-- TODO: move to ZkFold.Symbolic.Data.Bool

instance (SymbolicData x, Symbolic c) => Eq (x c) where
    type BooleanOf (x c) = Bool c
    x == y =
        let
            result = symbolic2F (toContext x) (toContext y)
                (mzipWithRep (\i j -> bool zero one (i Haskell.== j)))
                (\x' y' -> do
                    difference <- for (mzipRep x' y') $ \(i, j) ->
                        newAssigned (\w -> w i - w j)
                    (isZeros, _) <- runInvert difference
                    return isZeros
                )
        in
            all fromContext (unpacked result)

    x /= y =
        let
            result = symbolic2F (toContext x) (toContext y)
                (mzipWithRep (\i j -> bool zero one (i Haskell./= j)))
                (\x' y' -> do
                    difference <- for (mzipRep x' y') $ \(i, j) ->
                        newAssigned (\w -> w i - w j)
                    (isZeros, _) <- runInvert difference
                    for isZeros $ \isZ ->
                      newAssigned (\w -> one - w isZ)
                )
        in
            any fromContext (unpacked result)
