module ZkFold.Symbolic.Algorithm.Hash.Poseidon (
  module ZkFold.Symbolic.Algorithm.Hash.Poseidon,
  module ZkFold.Algorithm.Hash.Poseidon,
) where

import Data.Foldable (toList)
import Data.Function ((.))
import Data.Functor (fmap)

import ZkFold.Algorithm.Hash.Poseidon
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.Package (unpacked)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Symbolic.Class (Symbolic)

-- | Symbolic Poseidon hash
hash :: (SymbolicData x, Symbolic c) => x c -> FieldElement c
hash =
  poseidonHashDefault
    . fmap FieldElement
    . unpacked
    . hmap toList
    . arithmetize
