module ZkFold.Symbolic.Examples.LEQ (exampleLEQ) where

import ZkFold.Symbolic.Compat (CompatData)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Ord ((<=))
import ZkFold.Symbolic.V2 (Symbolic)

-- | (<=) operation
exampleLEQ
  :: Symbolic c
  => CompatData FieldElement c -> CompatData FieldElement c -> CompatData Bool c
exampleLEQ x y = x <= y
