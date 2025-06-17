module ZkFold.Symbolic.Examples.Conditional (exampleConditional) where

import           ZkFold.Control.Conditional        (ifThenElse)
import           ZkFold.Symbolic.Class             (Symbolic)
import           ZkFold.Symbolic.Data.Bool         (Bool)
import           ZkFold.Symbolic.Data.FieldElement (FieldElement)

exampleConditional ::
  Symbolic c => Bool c -> FieldElement c -> FieldElement c -> FieldElement c
exampleConditional = ifThenElse
