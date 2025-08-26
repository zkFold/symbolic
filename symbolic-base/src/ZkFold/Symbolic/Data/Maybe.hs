{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
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

import GHC.Generics (Generic, Generic1)
import Prelude (foldr, ($))
import qualified Prelude as Haskell

import ZkFold.Data.Eq
import ZkFold.Data.HFunctor.Classes (HEq)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class

data Maybe x c = Maybe {isJust :: Bool c, fromJust :: x c}
  deriving (Generic, Generic1, SymbolicData)

deriving stock instance (HEq c, Haskell.Eq (x c)) => Haskell.Eq (Maybe x c)

instance (SymbolicEq x c, Symbolic c) => Eq (Maybe x c)

just :: Symbolic c => x c -> Maybe x c
just = Maybe true

nothing :: forall x c . (SymbolicData x, HasRep x c, Symbolic c) => Maybe x c
nothing = Maybe false dummy

guard :: Bool c -> x c -> Maybe x c
guard = Maybe

fromMaybe :: forall c x. Conditional (Bool c) (x c) => x c -> Maybe x c -> x c
fromMaybe a (Maybe j t) = bool a t j

isNothing :: Symbolic c => Maybe x c -> Bool c
isNothing (Maybe h _) = not h

maybe :: forall a b c. Conditional (Bool c) (b c) => b c -> (a c -> b c) -> Maybe a c -> b c
maybe d h (Maybe j x) = fromMaybe d $ Maybe j (h x)

find
  :: forall a c t
   . (SymbolicData a, HasRep a c, Symbolic c, Haskell.Foldable t)
  => (a c -> Bool c)
  -> t (a c)
  -> Maybe a c
find p =
  let n = nothing
   in foldr (\i r -> maybe @a @_ @c (bool @(Bool c) n (just i) $ p i) (Haskell.const r) r) n
