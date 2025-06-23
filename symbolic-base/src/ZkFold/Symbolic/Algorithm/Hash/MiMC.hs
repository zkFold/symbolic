module ZkFold.Symbolic.Algorithm.Hash.MiMC (
  module ZkFold.Symbolic.Algorithm.Hash.MiMC,
  module ZkFold.Algorithm.Hash.MiMC,
) where

import Data.Foldable (toList)
import Data.Function (flip, (.))
import Data.Functor (fmap)
import Data.Proxy (Proxy (..))
import ZkFold.Algebra.Class
import ZkFold.Algorithm.Hash.MiMC
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.Package (unpacked)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement

hash :: SymbolicOutput x => x -> FieldElement (Context x)
hash =
  mimcHashN mimcConstants zero
    . fmap FieldElement
    . unpacked
    . hmap toList
    . flip arithmetize Proxy
