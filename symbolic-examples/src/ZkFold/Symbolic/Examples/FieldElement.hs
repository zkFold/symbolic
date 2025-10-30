module ZkFold.Symbolic.Examples.FieldElement (exampleInvert) where

import ZkFold.Algebra.Class (finv)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.V2 (Symbolic)
import ZkFold.Symbolic.Compat (CompatData)

exampleInvert
  :: Symbolic c => CompatData FieldElement c -> CompatData FieldElement c
exampleInvert = finv
