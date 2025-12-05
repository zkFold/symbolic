module ZkFold.Symbolic.Examples.LEQ (exampleLEQ) where

import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Ord ((<=))
import ZkFold.Symbolic.Class (Symbolic)

-- | (<=) operation
exampleLEQ :: Symbolic c => FieldElement c -> FieldElement c -> Bool c
exampleLEQ x y = x <= y
