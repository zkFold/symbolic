module Examples.FieldElement (exampleInvert) where

import           ZkFold.Base.Algebra.Basic.Class   (finv)
import           ZkFold.Symbolic.Class             (Symbolic)
import           ZkFold.Symbolic.Data.FieldElement (FieldElement)

exampleInvert :: Symbolic c => FieldElement c -> FieldElement c
exampleInvert = finv
