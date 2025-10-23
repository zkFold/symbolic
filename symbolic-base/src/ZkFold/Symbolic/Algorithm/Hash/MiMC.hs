module ZkFold.Symbolic.Algorithm.Hash.MiMC (
  module ZkFold.Symbolic.Algorithm.Hash.MiMC,
  module ZkFold.Algorithm.Hash.MiMC,
) where

import Data.Foldable (toList)
import Data.Function ((.))
import Data.Functor (fmap)

import ZkFold.Algebra.Class
import ZkFold.Algorithm.Hash.MiMC
import ZkFold.Data.Collect (Collect, collect)
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.Package (unpacked)
import qualified ZkFold.Symbolic.Class as Old
import ZkFold.Symbolic.Compat (CompatData)
import qualified ZkFold.Symbolic.Data.Class as Old
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import qualified ZkFold.Symbolic.Data.Unconstrained as C
import ZkFold.Symbolic.V2 (Symbolic)

hash :: (Old.SymbolicData x, Old.Symbolic c) => x c -> FieldElement c
hash =
  mimcHashN mimcConstants zero
    . fmap FieldElement
    . unpacked
    . hmap toList
    . Old.arithmetize

hashV2
  :: forall c a
   . (Collect (ConstrainedDatum c) a, Symbolic c)
  => a -> CompatData FieldElement c
hashV2 = mimcHashN mimcConstants zero . C.toList . collect @(ConstrainedDatum c)
