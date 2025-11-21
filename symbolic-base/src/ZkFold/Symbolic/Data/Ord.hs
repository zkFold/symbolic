{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Symbolic.Data.Ord (
  IsOrdering (..),
  Ordering,
  Ord (..),
  GOrd (..),
) where

import Control.DeepSeq (NFData)
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (zipWith)
import Data.Traversable (Traversable)
import GHC.Generics
import Prelude (Monoid, Semigroup, Show, ($), (.), (<>))
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Data.Eq
import ZkFold.Data.Ord
import ZkFold.Data.Orphans ()
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Vec (Vec (..))
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Class (Symbolic)
import Data.Semialign (Semialign)

newtype Ordering c = Ordering c
  deriving stock (Generic, Show)
  deriving anyclass NFData
  deriving SymbolicData via (Vec Par1)
  deriving (Eq, Ord) via (Vec Par1 c)

instance Symbolic c => Semigroup (Ordering c) where
  Ordering _o1 <> Ordering _o2 = Ordering $ Prelude.error "TODO"
    -- fromCircuit2F o1 o2 $
    --   \(Par1 v1) (Par1 v2) ->
    --     Par1
    --       <$> newAssigned (\x -> let x1 = x v1; x2 = x v2 in x1 * x1 * (x1 - x2) + x2)

instance Symbolic c => Monoid (Ordering c) where
  mempty = eq

instance Symbolic c => IsOrdering (Ordering c) where
  lt = Ordering (negate one)
  eq = Ordering zero
  gt = Ordering one

instance Symbolic c => Ord (Bool c) where
  type OrderingOf (Bool c) = Ordering c
  ordering x y z o = bool (bool x y (o == eq)) z (o == gt)
  compare (Bool _b1) (Bool _b2) = Ordering $ Prelude.error "TODO"
    -- fromCircuit2F b1 b2 $
    --   \(Par1 v1) (Par1 v2) -> fmap Par1 $
    --     newAssigned $
    --       \x -> let x1 = x v1; x2 = x v2 in x1 - x2

instance (Symbolic c, Semialign f, Traversable f) => Ord (Vec f c) where
  type OrderingOf (Vec f c) = Ordering c
  ordering x y z o = bool (bool x y (o == eq)) z (o == gt)
  compare = bitwiseCompare `on` (getBitsBE . runVec)

bitwiseCompare :: forall c. Symbolic c => [c] -> [c] -> Ordering c
bitwiseCompare x y = fold (zipWith (compare `on` Bool) x y)

getBitsBE :: forall c f. Symbolic c => f c -> [c]
-- ^ @getBitsBE x@ returns a list of circuits computing bits of @x@, eldest to
-- youngest.
getBitsBE _x = Prelude.error "TODO"
  -- symbolicF
  --   x
  --   (concatMap (reverse . padBits n . map fromConstant . binaryExpansion . toConstant))
  --   (fmap (concatMap reverse) . traverse (expansion n) . toList)
 where
  _n = numberOfBits @c
