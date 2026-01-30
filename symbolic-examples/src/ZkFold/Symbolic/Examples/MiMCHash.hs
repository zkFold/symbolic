module ZkFold.Symbolic.Examples.MiMCHash (exampleMiMC) where

import ZkFold.Algebra.Class (zero)
import ZkFold.Symbolic.Algorithm.Hash.MiMC (mimcConstants, mimcHash2S)
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)

-- | Optimized MiMC hash example using the Symbolic-specific implementation
-- that computes each round with only 3 polynomial constraints instead of 6+.
exampleMiMC :: Symbolic c => FieldElement c -> FieldElement c -> FieldElement c
exampleMiMC = mimcHash2S mimcConstants zero
