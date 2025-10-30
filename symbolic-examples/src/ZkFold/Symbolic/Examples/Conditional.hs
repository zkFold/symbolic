module ZkFold.Symbolic.Examples.Conditional (exampleConditional) where

import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Symbolic.Compat (CompatData)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.V2 (Symbolic)

exampleConditional
  :: Symbolic c
  => CompatData Bool c
  -> CompatData FieldElement c
  -> CompatData FieldElement c
  -> CompatData FieldElement c
exampleConditional = ifThenElse
