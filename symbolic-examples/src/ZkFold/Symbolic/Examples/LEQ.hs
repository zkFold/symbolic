module ZkFold.Symbolic.Examples.LEQ (exampleLEQ) where

import           ZkFold.Symbolic.Class             (Symbolic)
import           ZkFold.Symbolic.Data.Bool         (Bool)
import           ZkFold.Symbolic.Data.FieldElement (FieldElement)
import           ZkFold.Symbolic.Data.Ord          ((<=))

-- | (<=) operation
exampleLEQ :: Symbolic c => FieldElement c -> FieldElement c -> Bool c
exampleLEQ x y = x <= y
