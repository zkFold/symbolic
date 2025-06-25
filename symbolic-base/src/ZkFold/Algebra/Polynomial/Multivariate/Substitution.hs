module ZkFold.Algebra.Polynomial.Multivariate.Substitution where

import Data.Map (Map, lookup)
import Prelude hiding (
  Num (..),
  length,
  lookup,
  product,
  replicate,
  sum,
  (!!),
  (^),
 )

import ZkFold.Algebra.Field (Zp, fromZp)
import ZkFold.Algebra.Polynomial.Multivariate.Monomial
import ZkFold.Data.Vector (Vector, fromVector)
import ZkFold.Prelude ((!!))

-- | Data structure `s` can be viewed as a substitution from `i` to `b`
class Substitution s i b where
  subs :: s -> (i -> b)

instance Variable i => Substitution (Map i b) i (Maybe b) where
  subs = flip lookup

instance Substitution (Vector n b) (Zp n) b where
  subs v i = fromVector v !! fromZp i
