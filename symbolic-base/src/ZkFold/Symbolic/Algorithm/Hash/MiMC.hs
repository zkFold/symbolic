module ZkFold.Symbolic.Algorithm.Hash.MiMC (
  module ZkFold.Symbolic.Algorithm.Hash.MiMC,
  module ZkFold.Algorithm.Hash.MiMC,
) where

import Data.Function ((.))

import ZkFold.Algebra.Class
import ZkFold.Algorithm.Hash.MiMC
import ZkFold.Data.Collect (Collect, collect)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import qualified ZkFold.Symbolic.Data.Unconstrained as C

hash
  :: forall c a
   . (Collect (ConstrainedDatum c) a, Symbolic c)
  => a -> FieldElement c
hash = mimcHashN mimcConstants zero . C.toList . collect @(ConstrainedDatum c)
