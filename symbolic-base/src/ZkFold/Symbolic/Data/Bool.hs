{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.Bool (
  BoolType (..),
  Bool (..),
  Conditional (..),
  all,
  any,
  and,
  or,
  assert,
) where

import Data.Function (($))
import Data.List.NonEmpty (NonEmpty (..))

import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Bool
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Class
import Data.Functor ((<$>))
import ZkFold.Symbolic.Boot (Bool (..), FieldElement (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import ZkFold.Data.Eq ((==))
import ZkFold.Symbolic.Data.Ord (Ord (..), Ordering (..))

instance SymbolicData Bool

instance SymbolicInput Bool where
  isValid (Bool (FieldElement -> v)) = v * (v - one) == zero

instance Symbolic c => Ord (Bool c) where
  type OrderingOf (Bool c) = Ordering c
  compare (Bool (FieldElement -> u)) (Bool (FieldElement -> v)) =
    MkOrdering $ fromFieldElement (u - v)

instance (Symbolic c, SymbolicData d) => Conditional (Bool c) (d c) where
  bool onFalse onTrue (Bool b) =
    interpolate b ((zero, onFalse) :| [(one, onTrue)])

assert :: (SymbolicData h, Symbolic c) => (h c -> Bool c) -> h c -> h c
assert p x = fromLayout $ constrain (($ fromBool (p x)) =!= one) <$> toLayout x
