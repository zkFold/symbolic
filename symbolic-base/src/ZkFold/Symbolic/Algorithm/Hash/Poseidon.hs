module ZkFold.Symbolic.Algorithm.Hash.Poseidon (
  module ZkFold.Symbolic.Algorithm.Hash.Poseidon,
  module ZkFold.Algorithm.Hash.Poseidon,
) where

import Data.Function ((.))

import ZkFold.Algorithm.Hash.Poseidon
import ZkFold.Data.Collect (Collect, collect)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import qualified ZkFold.Symbolic.Data.Unconstrained as C

-- | Symbolic Poseidon hash
hash :: (Symbolic c, Collect (ConstrainedDatum c) x) => x -> FieldElement c
hash = poseidonHashDefault . C.toList . collect
