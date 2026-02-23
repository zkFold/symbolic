module ZkFold.Symbolic.Examples.PoseidonHash (examplePoseidon) where

import ZkFold.Algorithm.Hash.Poseidon (poseidonHashDefault)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)

-- | Poseidon hash example hashing two field elements.
examplePoseidon :: Symbolic c => FieldElement c -> FieldElement c -> FieldElement c
examplePoseidon x y = poseidonHashDefault [x, y]
