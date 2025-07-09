{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Maybe (
  Maybe,
  guard,
  maybe,
  just,
  nothing,
  fromMaybe,
  fromJust,
  isNothing,
  isJust,
  find,
) where

import Data.Functor ((<$>))
import Data.Functor.Rep (pureRep)
import GHC.Generics (Generic)
import Prelude (foldr, ($), type (~))
import qualified Prelude as Haskell

import ZkFold.Algebra.Class
import ZkFold.Data.Eq
import ZkFold.Data.HFunctor.Classes (HEq)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class

data Maybe context x = Maybe {isJust :: Bool context, fromJust :: x}
  deriving stock
    ( Generic
    , Haskell.Foldable
    , Haskell.Functor
    , Haskell.Traversable
    )

deriving stock instance (HEq c, Haskell.Eq x) => Haskell.Eq (Maybe c x)

instance (SymbolicData x, Context x ~ c) => SymbolicData (Maybe c x)

instance (Context x ~ c, SymbolicEq x) => Eq (Maybe c x)

just :: Symbolic c => x -> Maybe c x
just = Maybe true

nothing
  :: forall x c
   . (SymbolicData x, Context x ~ c)
  => Maybe c x
nothing =
  Maybe false $ restore (embed (pureRep zero), pureRep zero)

guard :: Bool c -> x -> Maybe c x
guard = Maybe

fromMaybe
  :: forall c x
   . Conditional (Bool c) x
  => x
  -> Maybe c x
  -> x
fromMaybe a (Maybe j t) = bool a t j

isNothing :: Symbolic c => Maybe c x -> Bool c
isNothing (Maybe h _) = not h

maybe
  :: forall a b c
   . Conditional (Bool c) b
  => b
  -> (a -> b)
  -> Maybe c a
  -> b
maybe d h m = fromMaybe d (h <$> m)

find
  :: forall a c t
   . (SymbolicData a, Context a ~ c, Haskell.Foldable t)
  => (a -> Bool c)
  -> t a
  -> Maybe c a
find p =
  let n = nothing
   in foldr (\i r -> maybe @a @_ @c (bool @(Bool c) n (just i) $ p i) (Haskell.const r) r) n
