module ZkFold.Symbolic.Algorithm.Hash.MiMC (
  module ZkFold.Symbolic.Algorithm.Hash.MiMC,
  module ZkFold.Algorithm.Hash.MiMC,
) where

import Data.Constraint (withDict)
import Data.Data (Proxy (Proxy))
import Data.Foldable (toList)
import Data.Function (($), (.))
import Data.Functor (fmap)

import ZkFold.Algebra.Class
import ZkFold.Algorithm.Hash.MiMC
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.Package (unpacked)
import ZkFold.Symbolic.Class (BaseField, Symbolic)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement

hash :: forall x c. (SymbolicData x, Symbolic c) => x c -> FieldElement c
hash =
  withDict (dataFunctor @x @(Order (BaseField c)) Proxy) $
    mimcHashN mimcConstants zero
      . fmap FieldElement
      . unpacked
      . hmap toList
      . arithmetize
