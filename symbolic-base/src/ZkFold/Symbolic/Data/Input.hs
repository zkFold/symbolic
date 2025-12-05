{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Data.Input where

import Data.Semialign (Semialign)
import qualified GHC.Generics as G
import Prelude (Foldable, (.))

import ZkFold.Data.Bool (all, true, (&&))
import ZkFold.Symbolic.Boot (Bool)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (HasRep, SymbolicData)

-- | A class for Symbolic input.
class SymbolicData d => SymbolicInput d where
  isValid :: (Symbolic c, HasRep d c) => d c -> Bool c
  default isValid
    :: (G.Generic1 d, SymbolicInput (G.Rep1 d), HasRep (G.Rep1 d) c, Symbolic c)
    => d c -> Bool c
  isValid = isValid . G.from1

instance SymbolicInput G.U1 where
  isValid _ = true

instance SymbolicInput G.Par1 where
  isValid _ = true

instance (SymbolicInput x, SymbolicInput y) => SymbolicInput (x G.:*: y) where
  isValid (x G.:*: y) = isValid x && isValid y

instance (Semialign f, Foldable f, SymbolicInput x) => SymbolicInput (f G.:.: x) where
  isValid = all isValid . G.unComp1

instance SymbolicInput x => SymbolicInput (G.M1 i c x) where
  isValid (G.M1 x) = isValid x

instance SymbolicInput x => SymbolicInput (G.Rec1 x) where
  isValid (G.Rec1 x) = isValid x
