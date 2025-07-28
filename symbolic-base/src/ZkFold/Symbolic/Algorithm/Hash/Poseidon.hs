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

-- | Symbolic Poseidon hash function
-- Converts symbolic data to field elements and applies Poseidon hash
hash :: SymbolicData x => x -> FieldElement (Context x)
hash =
  poseidonHashDefault
    . fmap FieldElement
    . unpacked
    . hmap toList
    . arithmetize

-- | Symbolic Poseidon hash with custom parameters
hashWith :: SymbolicData x => PoseidonParams (FieldElement (Context x)) -> x -> FieldElement (Context x)
hashWith params =
  poseidonHash params
    . fmap FieldElement
    . unpacked
    . hmap toList
    . arithmetize
