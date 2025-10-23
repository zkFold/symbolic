{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Unconstrained where

import Data.Foldable (Foldable, foldr)
import Data.Function (const, id, (.))
import Data.Functor (fmap, (<$>))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import GHC.Generics (Par1 (..))
import Text.Show (Show)

import ZkFold.Data.Collect (Collect (..))
import ZkFold.Symbolic.Compat (CompatContext (CompatContext, compatContext), CompatData (..), assert)
import ZkFold.Symbolic.Data.Bool (Bool)
import qualified ZkFold.Symbolic.Data.Class as Old
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.V2 (SymbolicData (..))
import ZkFold.Symbolic.Data.Witness (Witness (..))
import ZkFold.Symbolic.V2 (Symbolic)

newtype Unconstrained a c = Unconstrained {unconstrained :: a (Witness c)}

deriving instance Show (a (Witness c)) => Show (Unconstrained a c)

instance SymbolicData a => SymbolicData (Unconstrained a) where
  type Layout (Unconstrained a) c = Layout a (Witness c)
  type HasRep (Unconstrained a) c = HasRep a (Witness c)
  fromLayout = Unconstrained . fromLayout . fmap Witness
  interpolate c =
    Unconstrained . interpolate (Witness c) . fmap (unconstrained <$>)
  toLayout = fmap witness . toLayout . unconstrained

unconstrain :: (SymbolicData a, Symbolic c) => a c -> Unconstrained a c
unconstrain = Unconstrained . fromLayout . fmap Witness . toLayout

constrained
  :: (SymbolicData a, Symbolic c)
  => (a c -> CompatData Bool c) -> Unconstrained a c -> a c
constrained pred =
  assert pred . fromLayout . fmap witness . toLayout . unconstrained

newtype ConstrainedDatum c = ConstrainedDatum
  {constrainedDatum :: forall b. (CompatData FieldElement c -> b -> b) -> b -> b}

instance Semigroup (ConstrainedDatum c) where
  ConstrainedDatum f <> ConstrainedDatum g = ConstrainedDatum \i -> f i . g i

instance Monoid (ConstrainedDatum c) where
  mempty = ConstrainedDatum (const id)

single :: CompatData FieldElement c -> ConstrainedDatum c
single x = ConstrainedDatum \f s -> f x s

toDatum
  :: Foldable f => (a -> CompatData FieldElement c) -> f a -> ConstrainedDatum c
toDatum g as = ConstrainedDatum \f s -> foldr (f . g) s as

toList :: ConstrainedDatum c -> [CompatData FieldElement c]
toList (ConstrainedDatum k) = k (:) []

instance Collect (ConstrainedDatum c) (Unconstrained a c) where
  collect = const mempty

instance (Symbolic c, Old.SymbolicData a) => Collect (ConstrainedDatum c) (CompatData a c) where
  collect =
    toDatum (CompatData . FieldElement . CompatContext . Par1)
      . compatContext
      . Old.arithmetize
      . compatData
