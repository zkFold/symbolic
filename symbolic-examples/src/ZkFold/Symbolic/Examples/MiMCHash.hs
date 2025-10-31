module ZkFold.Symbolic.Examples.MiMCHash (exampleMiMC) where

import ZkFold.Algebra.Class
import ZkFold.Symbolic.Algorithm.Hash.MiMC (mimcConstants, mimcHash2)
import ZkFold.Symbolic.Compat (CompatData)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.V2 (Symbolic)

exampleMiMC
  :: Symbolic c
  => CompatData FieldElement c
  -> CompatData FieldElement c
  -> CompatData FieldElement c
exampleMiMC = mimcHash2 mimcConstants zero
