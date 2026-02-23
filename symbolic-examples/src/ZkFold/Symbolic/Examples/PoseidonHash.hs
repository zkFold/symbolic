module ZkFold.Symbolic.Examples.PoseidonHash (examplePoseidon) where

import ZkFold.Symbolic.Algorithm.Hash.Poseidon (poseidonHash2)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)

-- | Poseidon hash example hashing two field elements.
examplePoseidon :: Symbolic c => FieldElement c -> FieldElement c -> FieldElement c
examplePoseidon = poseidonHash2
