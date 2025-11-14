{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Input (
  SymbolicInput (..),
) where

import Data.Semialign (Semialign)
import qualified GHC.Generics as G
import Prelude (($), (.), Functor, Foldable)

import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Vec
import ZkFold.Symbolic.Data.V2 (SymbolicData (..))
import ZkFold.Symbolic.V2 (Symbolic)
import GHC.Err (error)

-- | A class for Symbolic input.
class SymbolicData d => SymbolicInput d where
  isValid :: (Symbolic c, HasRep d c) => d c -> Bool c
  default isValid
    :: (G.Generic1 d, SymbolicInput (G.Rep1 d), HasRep (G.Rep1 d) c, Symbolic c)
    => d c -> Bool c
  isValid = isValid . G.from1

instance SymbolicInput Bool where
  isValid (Bool _b) = Bool $ error "TODO"
    -- fromCircuitF b $
    --  \(G.Par1 v) -> do
    --    u <- newAssigned (\x -> x v * (one - x v))
    --    isZero $ G.Par1 u

instance Functor f => SymbolicInput (Vec f) where
  isValid _ = true

instance SymbolicInput G.U1 where
  isValid _ = true

instance (SymbolicInput x, SymbolicInput y) => SymbolicInput (x G.:*: y) where
  isValid (x G.:*: y) = isValid x && isValid y

instance (Semialign f, Foldable f, SymbolicInput x) => SymbolicInput (f G.:.: x) where
  isValid = all isValid . G.unComp1

instance SymbolicInput x => SymbolicInput (G.M1 i c x) where
  isValid (G.M1 x) = isValid x

instance SymbolicInput x => SymbolicInput (G.Rec1 x) where
  isValid (G.Rec1 x) = isValid x
