{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Data.Witness where

import           ZkFold.Symbolic.Class      (Symbolic(..), embedW)
import           ZkFold.Symbolic.Data.Bool  (true, Bool)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import GHC.Generics ((:.:) (..))
import Data.Functor (fmap)
import Data.Function (($))
import ZkFold.Data.Package (pack, unpack)
import ZkFold.Symbolic.Data.Conditional (Conditional (..))
import ZkFold.Prelude (zipWithDefault)
import ZkFold.Algebra.Class (zero)
import Data.Functor.Rep (pureRep)

newtype Wit x c = Wit { runWit :: Layout x (BaseField c) (WitnessField c) }

instance SymbolicDataConstraint x => SymbolicData (Wit x) where
  type Layout (Wit x) a = Layout x a
  toContext (Wit w) = embedW w
  fromContext c = Wit (witnessF c)

instance SymbolicDataConstraint x => SymbolicInput (Wit x) where
  isValid _ = true

instance SymbolicData (Wit x) => SymbolicData ([] :.: Wit x) where
  type Layout ([] :.: Wit x) a = [] :.: Layout (Wit x) a
  toContext (Comp1 xs) = pack $ fmap toContext xs
  fromContext c = Comp1 $ fmap fromContext (unpack c)

instance SymbolicDataConstraint x => SymbolicInput ([] :.: Wit x) where
  isValid _ = true

instance (SymbolicDataConstraint x, Symbolic c) => Conditional (Bool c) [Wit x c] where
  bool xs ys b = zipWithDefault (\x y -> bool x y b) (Wit $ pureRep zero) (Wit $ pureRep zero) xs ys
