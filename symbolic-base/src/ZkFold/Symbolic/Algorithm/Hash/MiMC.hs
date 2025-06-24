module ZkFold.Symbolic.Algorithm.Hash.MiMC (
  module ZkFold.Symbolic.Algorithm.Hash.MiMC,
  module ZkFold.Algorithm.Hash.MiMC,
) where

import Data.Foldable (toList)
import Data.Function ((.))
import Data.Functor (fmap)

import ZkFold.Algebra.Class
import ZkFold.Algorithm.Hash.MiMC
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.Package (unpacked)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement

hash :: SymbolicData x => x -> FieldElement (Context x)
hash =
  mimcHashN mimcConstants zero
    . fmap FieldElement
    . unpacked
    . hmap toList
    . arithmetize
