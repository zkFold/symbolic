module ZkFold.Symbolic.Examples.LEQ (exampleLEQ) where

import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Ord ((<=))
import ZkFold.Symbolic.V2 (Symbolic)
import ZkFold.Symbolic.Compat (CompatData)

-- | (<=) operation
exampleLEQ
  :: Symbolic c
  => CompatData FieldElement c -> CompatData FieldElement c -> CompatData Bool c
exampleLEQ x y = x <= y
