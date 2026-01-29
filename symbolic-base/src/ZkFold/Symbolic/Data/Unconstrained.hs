{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Unconstrained where

import Data.Foldable (Foldable, foldr)
import Data.Function (const, id, (.))
import Data.Functor (fmap, (<$>))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Text.Show (Show)

import ZkFold.Data.Collect (Collect (..))
import ZkFold.Symbolic.Boot (FieldElement (..))
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool (..), assert)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Witness (Witness (..))

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
  => (a c -> Bool c) -> Unconstrained a c -> a c
constrained pred =
  assert pred . fromLayout . fmap witness . toLayout . unconstrained

newtype ConstrainedDatum c = ConstrainedDatum
  {constrainedDatum :: forall b. (FieldElement c -> b -> b) -> b -> b}

instance Semigroup (ConstrainedDatum c) where
  ConstrainedDatum f <> ConstrainedDatum g = ConstrainedDatum \i -> f i . g i

instance Monoid (ConstrainedDatum c) where
  mempty = ConstrainedDatum (const id)

single :: FieldElement c -> ConstrainedDatum c
single x = ConstrainedDatum \f s -> f x s

toDatum
  :: Foldable f => (a -> FieldElement c) -> f a -> ConstrainedDatum c
toDatum g as = ConstrainedDatum \f s -> foldr (f . g) s as

toList :: ConstrainedDatum c -> [FieldElement c]
toList (ConstrainedDatum k) = k (:) []

instance Collect (ConstrainedDatum c) (Unconstrained a c) where
  collect = const mempty

instance Collect (ConstrainedDatum c) (FieldElement c) where
  collect = single

deriving via (FieldElement c) instance Collect (ConstrainedDatum c) (Bool c)
