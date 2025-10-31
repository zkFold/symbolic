module ZkFold.Symbolic.Examples.MiMCHash (exampleMiMC) where

import ZkFold.Algebra.Class
import ZkFold.Symbolic.Algorithm.Hash.MiMC (mimcConstants, mimcHash2)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.V2 (Symbolic)
import ZkFold.Symbolic.Compat (CompatData)

exampleMiMC
  :: Symbolic c
  => CompatData FieldElement c
  -> CompatData FieldElement c
  -> CompatData FieldElement c
exampleMiMC = mimcHash2 mimcConstants zero
