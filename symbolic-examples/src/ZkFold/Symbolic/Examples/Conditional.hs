module ZkFold.Symbolic.Examples.Conditional (exampleConditional) where

import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.V2 (Symbolic)
import ZkFold.Symbolic.Compat (CompatData)

exampleConditional
  :: Symbolic c
  => CompatData Bool c
  -> CompatData FieldElement c
  -> CompatData FieldElement c
  -> CompatData FieldElement c
exampleConditional = ifThenElse
