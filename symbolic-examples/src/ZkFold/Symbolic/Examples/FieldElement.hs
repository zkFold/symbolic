module ZkFold.Symbolic.Examples.FieldElement (exampleInvert) where

import ZkFold.Algebra.Class (finv)
import ZkFold.Symbolic.Compat (CompatData)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.V2 (Symbolic)

exampleInvert
  :: Symbolic c => CompatData FieldElement c -> CompatData FieldElement c
exampleInvert = finv
