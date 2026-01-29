{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Ord (
  Ordering (..),
  module ZkFold.Data.Ord,
  unconstrainedCompare,
) where

import Control.Applicative (pure)
import Data.Functor (Functor, (<$>))
import GHC.Generics
import Test.QuickCheck (Arbitrary (..), oneof)
import Prelude (Monoid, Semigroup, ($), (<>))
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq
import ZkFold.Data.Ord
import ZkFold.Data.Orphans ()
import ZkFold.Symbolic.Boot (FieldElement (..))
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))

newtype Ordering c = MkOrdering {fromOrdering :: c}
  deriving stock (Functor, Generic1)
  deriving anyclass SymbolicData
  deriving Eq via FieldElement c

instance SymbolicInput Ordering where
  isValid (MkOrdering (FieldElement -> x)) = x * x * x == x

instance Symbolic c => Semigroup (Ordering c) where
  MkOrdering (FieldElement -> x) <> MkOrdering (FieldElement -> y) =
    MkOrdering $ fromFieldElement (x + y * (one - x * x))

instance Symbolic c => Monoid (Ordering c) where
  mempty = eq

instance Symbolic c => IsOrdering (Ordering c) where
  lt = MkOrdering (negate one)
  eq = MkOrdering zero
  gt = MkOrdering one

instance Symbolic c => Arbitrary (Ordering c) where
  arbitrary = oneof $ pure <$> [lt, eq, gt]

unconstrainedCompare :: Symbolic c => c -> c -> Ordering c
unconstrainedCompare x y =
  MkOrdering $
    ifThenElse (x == y) (fromOrdering eq) $
      ifThenElse (toIntegral x < toIntegral y) (fromOrdering lt) (fromOrdering gt)
