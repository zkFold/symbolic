{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Sum where

import Control.Applicative ((<*>))
import Data.Either (Either (..), either)
import Data.Function (const, ($), (.))
import Data.Functor (fmap, (<$>))
import Data.Functor.Rep (tabulate)
import Data.List (zipWith)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy)
import Data.Type.Equality (type (~))
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import ZkFold.Algebra.Class
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement (FieldElement, fromFieldElement)
import ZkFold.Symbolic.Data.Vec (runVec)

type family Product ts c where
  Product '[] c = Proxy c
  Product (t ': ts) c = (t, Product ts c)

type family Eithers ts where
  Eithers '[] = Void
  Eithers (t ': ts) = Either t (Eithers ts)

data OneOf ts c = OneOf
  { discriminant :: FieldElement c
  , branches :: Product ts c
  }
  deriving Generic

instance
  ( Symbolic c
  , SymbolicData (Product ts c)
  , Context (Product ts c) ~ c
  , Support (Product ts c) ~ Proxy c
  )
  => SymbolicData (OneOf ts c)

class
  ( SymbolicData (Product ts c)
  , Context (Product ts c) ~ c
  , Support (Product ts c) ~ Proxy c
  ) =>
  Embed ts c
  where
  embed :: Eithers ts -> Product ts c
  indexOf :: Eithers ts -> Natural
  enum :: Product ts c -> [Eithers ts]

instance Symbolic c => Embed '[] c where
  embed = absurd
  indexOf = absurd
  enum = const []

zeroed :: SymbolicData a => a
zeroed = restore $ const (runVec zero, tabulate zero)

instance (SymbolicOutput t, Context t ~ c, Embed ts c) => Embed (t ': ts) c where
  embed = either (,zeroed) ((zeroed,) . embed @ts @c)
  indexOf = either zero ((+ one) . indexOf @ts @c)
  enum (h, t) = Left h : fmap Right (enum @ts @c t)

embedOneOf :: forall ts c. Embed ts c => Eithers ts -> OneOf ts c
embedOneOf = OneOf <$> fromConstant . indexOf @ts @c <*> embed @ts @c

matchOneOf
  :: forall r ts
   . (SymbolicData r, Embed ts (Context r))
  => OneOf ts (Context r) -> (Eithers ts -> r) -> r
matchOneOf OneOf {..} f =
  case zipWith
    (\c b -> (fromConstant c, f b))
    [(0 :: Natural) ..]
    (enum @ts @(Context r) branches) of
    [] -> zeroed
    (b : bs) -> interpolate (b :| bs) (fromFieldElement discriminant)
