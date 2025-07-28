module ZkFold.Symbolic.Algorithm.Hash.Poseidon (
  module ZkFold.Symbolic.Algorithm.Hash.Poseidon,
  module ZkFold.Algorithm.Hash.Poseidon,
) where

import Data.Foldable (toList)
import Data.Function ((.))
import Data.Functor (fmap)

import ZkFold.Algebra.Class
import ZkFold.Algorithm.Hash.Poseidon
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.Package (unpacked)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement

-- | Symbolic Poseidon hash
hash :: SymbolicData x => x -> FieldElement (Context x)
hash =
  poseidonHashDefault
    . fmap FieldElement
    . unpacked
    . hmap toList
    . arithmetize