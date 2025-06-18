{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Witness where

import           ZkFold.Symbolic.Class      (Symbolic(..), embedW)
import           ZkFold.Symbolic.Data.Bool  (true)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Input (SymbolicInput (..))

newtype Wit x c = Wit { runWit :: Layout x (BaseField c) (WitnessField c) }

instance SymbolicDataConstraint x => SymbolicData (Wit x) where
  type Layout (Wit x) a = Layout x a
  toContext (Wit w) = embedW w
  fromContext c = Wit (witnessF c)

instance SymbolicDataConstraint x => SymbolicInput (Wit x) where
  isValid _ = true
