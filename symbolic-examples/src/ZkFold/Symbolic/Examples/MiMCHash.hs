module ZkFold.Symbolic.Examples.MiMCHash (exampleMiMC) where

import ZkFold.Algebra.Class
import ZkFold.Symbolic.Algorithm.Hash.MiMC (mimcConstants, mimcHash2)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)

exampleMiMC :: Symbolic c => FieldElement c -> FieldElement c -> FieldElement c
exampleMiMC = mimcHash2 mimcConstants zero
