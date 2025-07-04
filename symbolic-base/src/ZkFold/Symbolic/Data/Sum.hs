{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Sum where

import Control.Applicative ((<*>))
import Data.Bifunctor (first)
import Data.Either (Either (..), either)
import Data.Function (const, id, ($), (.))
import Data.Functor (fmap, (<$>))
import Data.Functor.Rep (tabulate)
import Data.Kind (Type)
import Data.List (zipWith)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Type.Equality (type (~))
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import qualified GHC.Generics as G
import Numeric.Natural (Natural)

import ZkFold.Algebra.Class
import ZkFold.Data.Eq (Eq)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (SymbolicEq)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement (FieldElement, fromFieldElement)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Vec (runVec)

------------------------------ Product & Eithers -------------------------------

type family Product ts c where
  Product '[] c = Proxy c
  Product (t ': ts) c = (t, Product ts c)

type family Eithers ts where
  Eithers '[] = Void
  Eithers (t ': ts) = Either t (Eithers ts)

class
  ( SymbolicData (Product ts c)
  , Context (Product ts c) ~ c
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

instance (SymbolicData t, Context t ~ c, Embed ts c) => Embed (t ': ts) c where
  embed = either (,zeroed) ((zeroed,) . embed @ts @c)
  indexOf = either zero ((+ one) . indexOf @ts @c)
  enum (h, t) = Left h : fmap Right (enum @ts @c t)

-- | A helper for producing default values.
zeroed :: SymbolicData a => a
zeroed = restore (runVec zero, tabulate zero)

--------------------------------- OneOf datatype -------------------------------

data OneOf ts c = OneOf
  { discriminant :: FieldElement c
  , branches :: Product ts c
  }
  deriving Generic

instance
  ( Symbolic c
  , SymbolicData (Product ts c)
  , Context (Product ts c) ~ c
  )
  => SymbolicData (OneOf ts c)

instance
  ( Symbolic c
  , SymbolicInput (Product ts c)
  , Context (Product ts c) ~ c
  )
  => SymbolicInput (OneOf ts c)

instance
  ( Symbolic c
  , SymbolicEq (Product ts c)
  , Context (Product ts c) ~ c
  ) => Eq (OneOf ts c)

embedOneOf :: forall ts c. Embed ts c => Eithers ts -> OneOf ts c
embedOneOf = OneOf <$> fromConstant . indexOf @ts @c <*> embed @ts @c

matchOneOf
  :: forall r ts
   . (SymbolicData r, Embed ts (Context r))
  => OneOf ts (Context r)
  -> (Eithers ts -> r)
  -> r
matchOneOf OneOf {..} f =
  case zipWith
    (\c b -> (fromConstant c, f b))
    [(0 :: Natural) ..]
    (enum @ts @(Context r) branches) of
    [] -> zeroed
    (b : bs) -> interpolate (b :| bs) (fromFieldElement discriminant)

------------------------------ Nested Product type family ----------------------

class (SymbolicData (NP f c), Context (NP f c) ~ c) => Produces f c where
  type NP f c :: Type
  produce :: f u -> NP f c
  utilize :: NP f c -> f u

instance (Produces f c, Produces g c) => Produces (f G.:*: g) c where
  type NP (f G.:*: g) c = (NP f c, NP g c)
  produce (f G.:*: g) = (produce f, produce g)
  utilize (x, y) = utilize x G.:*: utilize y

instance Symbolic c => Produces G.U1 c where
  type NP G.U1 c = Proxy c
  produce _ = Proxy
  utilize _ = G.U1

instance (SymbolicData a, Context a ~ c) => Produces (G.M1 G.S u (G.K1 v a)) c where
  type NP (G.M1 G.S u (G.K1 v a)) c = a
  produce (G.M1 (G.K1 a)) = a
  utilize a = G.M1 (G.K1 a)

---------------- Sum-of-Products form of a Symbolic datatype -------------------

class Embed (SOP f c '[]) c => Injects f c where
  type SOP f c (acc :: [Type]) :: [Type]
  sumFrom :: f u -> Proxy ts -> Eithers (SOP f c ts)
  sumSkip :: Proxy ts -> Eithers ts -> Eithers (SOP f c ts)
  sumTo :: Proxy ts -> Eithers (SOP f c ts) -> Either (f u) (Eithers ts)

sopify :: forall c f u. Injects f c => f u -> Eithers (SOP f c '[])
sopify f = sumFrom @_ @c f (Proxy @'[])

desop :: forall c f u. Injects f c => Eithers (SOP f c '[]) -> f u
desop = either id absurd . sumTo @_ @c (Proxy @'[])

instance
  (Injects f c, Injects g c, Embed (SOP f c (SOP g c '[])) c)
  => Injects (f G.:+: g) c
  where
  type SOP (f G.:+: g) c acc = SOP f c (SOP g c acc)
  sumFrom (G.L1 f) (_ :: Proxy ts) = sumFrom @_ @c f (Proxy @(SOP g c ts))
  sumFrom (G.R1 g) (_ :: Proxy ts) = sumSkip @f @c (Proxy @(SOP g c ts)) $ sumFrom @_ @c g (Proxy @ts)
  sumSkip (_ :: Proxy ts) es = sumSkip @f @c (Proxy @(SOP g c ts)) $ sumSkip @g @c (Proxy @ts) es
  sumTo (_ :: Proxy ts) =
    either (Left . G.L1) (first G.R1 . sumTo @g @c (Proxy @ts)) . sumTo @f @c (Proxy @(SOP g c ts))

instance Symbolic c => Injects G.V1 c where
  type SOP G.V1 c acc = acc
  sumFrom x = case x of {}
  sumSkip _ es = es
  sumTo _ = Right

instance Injects f c => Injects (G.M1 G.D u f) c where
  type SOP (G.M1 G.D u f) c acc = SOP f c acc
  sumFrom (G.M1 f) = sumFrom @_ @c f
  sumSkip = sumSkip @f @c
  sumTo p = first G.M1 . sumTo @f @c p

instance Produces f c => Injects (G.M1 G.C u f) c where
  type SOP (G.M1 G.C u f) c acc = NP f c ': acc
  sumFrom (G.M1 f) _ = Left (produce f)
  sumSkip _ = Right
  sumTo _ = first (G.M1 . utilize)

----------------------------- Sum wrapper datatype -----------------------------

type Cons a c = SOP (G.Rep a) c '[]

newtype Sum a c = Sum {sumOf :: OneOf (Cons a c) c}

type Prod a c = Product (Cons a c) c

deriving newtype instance
  ( Symbolic c
  , SymbolicData (Prod a c)
  , Context (Prod a c) ~ c
  )
  => SymbolicData (Sum a c)

deriving newtype instance
  ( Symbolic c
  , SymbolicInput (Prod a c)
  , Context (Prod a c) ~ c
  )
  => SymbolicInput (Sum a c)

deriving newtype instance
  ( Symbolic c
  , SymbolicEq (Prod a c)
  , Context (Prod a c) ~ c
  ) => Eq (Sum a c)

inject :: forall a c. (Generic a, Injects (G.Rep a) c) => a -> Sum a c
inject = Sum . embedOneOf . sopify @c . G.from

match :: forall a r. (SymbolicData r, Generic a, Injects (G.Rep a) (Context r)) => Sum a (Context r) -> (a -> r) -> r
match Sum {..} f = matchOneOf sumOf (f . G.to . desop @(Context r))
