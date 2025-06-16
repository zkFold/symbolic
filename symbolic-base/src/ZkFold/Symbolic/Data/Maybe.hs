{-# LANGUAGE DerivingStrategies #-}

module ZkFold.Symbolic.Data.Maybe (
    Maybe, maybe, just, nothing, fromMaybe, fromJust, isNothing, isJust, find
) where

import           Data.Foldable                    (Foldable)
import           Data.Function                    (const)
import           Data.Functor.Rep                 (pureRep)
import           GHC.Generics                     (Generic1)
import           Prelude                          (foldr, ($))
import qualified Prelude                          as Haskell

import           ZkFold.Algebra.Class
import           ZkFold.Data.HFunctor.Classes     (HEq)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Conditional

-- TODO: derive functor instances for `Maybe`

data Maybe x c = Maybe { isJust :: Bool c, fromJust :: x c }
  deriving Generic1

  -- deriving stock
  --   ( Haskell.Functor
  --   , Haskell.Foldable
  --   , Haskell.Traversable
  --   )

deriving stock instance (HEq c, Haskell.Eq (x c)) => Haskell.Eq (Maybe x c)

instance (SymbolicData x) => SymbolicData (Maybe x)

just :: Symbolic c => x c -> Maybe x c
just = Maybe true

nothing :: (SymbolicData x, Symbolic c) =>
  Maybe x c
nothing =
  Maybe false $ fromContext $ embed $ pureRep zero

fromMaybe :: (SymbolicData x, Symbolic c) =>
  x c -> Maybe x c -> x c
fromMaybe a (Maybe j t) = bool a t j

isNothing :: Symbolic c => Maybe x c -> Bool c
isNothing (Maybe h _) = not h

maybe :: (SymbolicData b, Symbolic c) => b c -> (a c -> b c) -> Maybe a c -> b c
maybe d h m =
  let Maybe {..} = m
  in fromMaybe d (just $ h fromJust)

find :: forall a c t .
    (SymbolicData a, Symbolic c, Foldable t) =>
    (a c -> Bool c) -> t (a c) -> Maybe a c
find p = let n = nothing in
    foldr (\i r -> maybe (bool n (just i) $ p i) (const r) r) n
