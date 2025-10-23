{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Maybe (
  Maybe,
  guard,
  maybe,
  mmap,
  just,
  nothing,
  fromMaybe,
  fromJust,
  isNothing,
  isJust,
  find,
) where

import Data.Function ((.))
import Data.Type.Equality (type (~))
import GHC.Generics (Generic, Generic1)
import Prelude (foldr)
import qualified Prelude as Haskell

import ZkFold.Data.Eq
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class (HasRep, SymbolicData, dummy)

data Maybe x c = Maybe {isJust :: Bool c, fromJust :: x c}
  deriving (Generic, Generic1, SymbolicData)

instance (Symbolic c, Eq (x c), BooleanOf (x c) ~ Bool c) => Eq (Maybe x c)

just :: Symbolic c => x c -> Maybe x c
just = Maybe true

nothing :: forall x c. (SymbolicData x, HasRep x c, Symbolic c) => Maybe x c
nothing = Maybe false dummy

guard :: Bool c -> x c -> Maybe x c
guard = Maybe

fromMaybe :: forall c x. Conditional (Bool c) (x c) => x c -> Maybe x c -> x c
fromMaybe a (Maybe j t) = bool a t j

isNothing :: Symbolic c => Maybe x c -> Bool c
isNothing = not . isJust

maybe :: forall a b c. Conditional (Bool c) (b c) => b c -> (a c -> b c) -> Maybe a c -> b c
maybe d h = fromMaybe d . mmap h

mmap :: (a c -> b c) -> Maybe a c -> Maybe b c
mmap f (Maybe b a) = Maybe b (f a)

find
  :: forall a c t
   . (SymbolicData a, HasRep a c, Symbolic c, Haskell.Foldable t)
  => (a c -> Bool c)
  -> t (a c)
  -> Maybe a c
find p =
  foldr (\i r -> maybe (bool nothing (just i) (p i)) (Haskell.const r) r) nothing
