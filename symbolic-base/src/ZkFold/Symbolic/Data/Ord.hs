{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.Ord (
  IsOrdering (..),
  Ordering,
  Ord (..),
  GOrd (..),
) where

import Control.DeepSeq (NFData)
import Data.Foldable (Foldable, fold, toList)
import Data.Function (on)
import Data.Functor (Functor)
import Data.List (concatMap, reverse, zipWith)
import Data.Traversable (traverse)
import GHC.Generics
import Prelude (Monoid, Semigroup, Show, fmap, map, ($), (.), (<$>), (<>))
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Data.Eq
import ZkFold.Data.HFunctor.Classes (HNFData, HShow)
import ZkFold.Data.Package
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators (expansion)
import ZkFold.Symbolic.Data.Vec (Vec (..))
import ZkFold.Symbolic.MonadCircuit (newAssigned)
import ZkFold.Data.Ord

newtype Ordering c = Ordering (c Par1)
  deriving Generic
  deriving SymbolicData via (Vec Par1)
  deriving (Eq, Ord) via (Vec Par1 c)

deriving instance HShow c => Show (Ordering c)

instance HNFData c => NFData (Ordering c)

instance Symbolic c => Semigroup (Ordering c) where
  Ordering o1 <> Ordering o2 = Ordering $
    fromCircuit2F o1 o2 $
      \(Par1 v1) (Par1 v2) ->
        Par1
          <$> newAssigned (\x -> let x1 = x v1; x2 = x v2 in x1 * x1 * (x1 - x2) + x2)

instance Symbolic c => Monoid (Ordering c) where
  mempty = eq

instance Symbolic c => IsOrdering (Ordering c) where
  lt = Ordering $ embed (Par1 (negate one))
  eq = Ordering $ embed (Par1 zero)
  gt = Ordering $ embed (Par1 one)

instance Symbolic c => Ord (Bool c) where
  type OrderingOf (Bool c) = Ordering c
  ordering x y z o = bool (bool x y (o == eq)) z (o == gt)
  compare (Bool b1) (Bool b2) = Ordering $
    fromCircuit2F b1 b2 $
      \(Par1 v1) (Par1 v2) -> fmap Par1 $
        newAssigned $
          \x -> let x1 = x v1; x2 = x v2 in x1 - x2

instance (Symbolic c, LayoutFunctor f) => Ord (Vec f c) where
  type OrderingOf (Vec f c) = Ordering c
  ordering x y z o = bool (bool x y (o == eq)) z (o == gt)
  compare = bitwiseCompare `on` (getBitsBE . runVec)

bitwiseCompare :: forall c. Symbolic c => c [] -> c [] -> Ordering c
bitwiseCompare x y = fold ((zipWith (compare `on` Bool) `on` unpacked) x y)

getBitsBE :: forall c f. (Symbolic c, Foldable f, Functor f) => c f -> c []
-- ^ @getBitsBE x@ returns a list of circuits computing bits of @x@, eldest to
-- youngest.
getBitsBE x =
  symbolicF
    x
    (concatMap (reverse . padBits n . map fromConstant . binaryExpansion . toConstant))
    (fmap (concatMap reverse) . traverse (expansion n) . toList)
 where
  n = numberOfBits @(BaseField c)
