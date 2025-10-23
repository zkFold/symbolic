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

import Control.Applicative (pure)
import qualified Data.Bool as Haskell
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List.NonEmpty (NonEmpty (..))
import Test.QuickCheck (Arbitrary (..), oneof)

import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Bool
import ZkFold.Data.Eq ((==))
import ZkFold.Symbolic.Boot (Bool (..), FieldElement (..))
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import ZkFold.Symbolic.Data.Ord (Ord (..), Ordering (..))
import GHC.Stack (HasCallStack)

instance SymbolicData Bool

instance SymbolicInput Bool where
  isValid (Bool (FieldElement -> v)) = v * (v - one) == zero

instance Symbolic c => Arbitrary (Bool c) where
  arbitrary = oneof $ pure <$> [false, true]

instance Symbolic c => Ord (Bool c) where
  type OrderingOf (Bool c) = Ordering c
  compare (Bool (FieldElement -> u)) (Bool (FieldElement -> v)) =
    MkOrdering $ fromFieldElement (u - v)

instance (Symbolic c, SymbolicData d) => Conditional (Bool c) (d c) where
  bool onFalse onTrue (Bool b) =
    interpolate b ((zero, onFalse) :| [(one, onTrue)])

instance Arithmetic a => ToConstant (Bool a) where
  type Const (Bool a) = Haskell.Bool
  toConstant (Bool b) = b == one

assert
  :: (SymbolicData h, Symbolic c, HasCallStack) => (h c -> Bool c) -> h c -> h c
assert p x = fromLayout $ constrain (($ fromBool (p x)) =!= one) <$> toLayout x
