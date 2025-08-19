{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Data.Input (
  SymbolicInput (..),
) where

import Data.Traversable (Traversable)
import Data.Typeable (Proxy)
import qualified GHC.Generics as G
import Prelude (($), (.))

import ZkFold.Algebra.Class
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.Vec
import ZkFold.Symbolic.MonadCircuit
import Data.Semialign (Semialign, Zip)

-- | A class for Symbolic input.
class SymbolicData d => SymbolicInput d where
  isValid :: Symbolic c => d c -> Bool c
  default isValid :: (G.Generic1 d, SymbolicInput (G.Rep1 d), Symbolic c) => d c -> Bool c
  isValid = isValid . G.from1

instance SymbolicInput Bool where
  isValid (Bool b) = Bool $
    fromCircuitF b $
      \(G.Par1 v) -> do
        u <- newAssigned (\x -> x v * (one - x v))
        isZero $ G.Par1 u

instance (Semialign f, Traversable f) => SymbolicInput (Vec f) where
  isValid _ = true

instance SymbolicInput Proxy where
  isValid _ = true

instance (SymbolicInput x, SymbolicInput y) => SymbolicInput (x G.:*: y) where
  isValid (x G.:*: y) = isValid x && isValid y

instance (Zip f, Traversable f, SymbolicInput x) => SymbolicInput (f G.:.: x) where
  isValid = all isValid . G.unComp1

instance SymbolicInput x => SymbolicInput (G.M1 i c x) where
  isValid (G.M1 x) = isValid x

instance SymbolicInput x => SymbolicInput (G.Rec1 x) where
  isValid (G.Rec1 x) = isValid x
