{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.FieldElement (FieldElement (..)) where

import Data.Function (on, (.))
import Data.Functor (fmap)

import ZkFold.Algebra.Class hiding (invert)
import ZkFold.Data.Eq (SemialignEqOrd (..))
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Boot (Bool (fromBool), FieldElement (..))
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Ord (Ord (..), Ordering)
import ZkFold.Symbolic.Data.Register (bitsOfFE)

instance SymbolicData FieldElement

instance SymbolicInput FieldElement

instance Symbolic c => BinaryExpansion (FieldElement c) where
  type Bits (FieldElement c) = Vector (NumberOfBits c) (FieldElement c)
  binaryExpansion = fmap (FieldElement . fromBool) . bitsOfFE

instance Symbolic c => Ord (FieldElement c) where
  type OrderingOf (FieldElement c) = Ordering c
  compare = compare `on` (MkSemialignEqOrd . bitsOfFE)
