{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Sum where

import Control.Applicative ((<*>))
import Data.Bifunctor (first)
import Data.Either (Either (..), either)
import Data.Function (const, id, ($), (.))
import Data.Functor (fmap, (<$>))
import Data.Functor.Rep (tabulate)
import Data.Kind (Type, Constraint)
import Data.List (zipWith)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import qualified GHC.Generics as G
import Numeric.Natural (Natural)

import ZkFold.Algebra.Class
import ZkFold.Data.Eq (Eq)
import ZkFold.Symbolic.Class (Symbolic, Ctx)
import ZkFold.Symbolic.Data.Bool (SymbolicEq)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement (FieldElement, fromFieldElement)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Vec (runVec)

------------------------------ Product & Eithers -------------------------------

type family Product ts where
  Product '[] = Proxy
  Product (t : ts) = t G.:*: Product ts

type family Eithers ts where
  Eithers '[] = G.V1
  Eithers (t : ts) = t G.:+: Eithers ts

type family RepProduct ts c :: Constraint where
  RepProduct '[] _ = ()
  RepProduct (t : ts) c = (RepData t c, RepData (Product ts) c, RepProduct ts c)

class SymbolicData (Product ts) => Embed ts where
  embed :: (RepProduct ts c, Symbolic c) => Proxy ts -> Eithers ts c -> Product ts c
  indexOf :: Proxy ts -> Eithers ts c -> Natural
  enum :: Proxy ts -> Product ts c -> [Eithers ts c]

instance Embed '[] where
  embed _ = \case
  indexOf _ = \case
  enum _ = const []

instance (SymbolicData t, Embed ts) => Embed (t ': ts) where
  embed _ = \case
    G.L1 x -> x G.:*: zeroed
    G.R1 x -> zeroed G.:*: embed @ts Proxy x
  -- either (,zeroed) ((zeroed,) . embed @ts)
  indexOf _ = \case
    G.L1 _ -> zero
    G.R1 x -> one + indexOf @ts Proxy x
  -- either zero ((+ one) . indexOf @ts)
  enum _ (h G.:*: t) = G.L1 h : fmap G.R1 (enum @ts Proxy t)

-- | A helper for producing default values.
zeroed :: (SymbolicData a, RepData a c, Symbolic c) => a c
zeroed = restore (runVec zero, tabulate zero)

--------------------------------- OneOf datatype -------------------------------

data OneOf ts c = OneOf
  { discriminant :: FieldElement c
  , branches :: Product ts c
  }
  deriving Generic

instance SymbolicData (Product ts) => SymbolicData (OneOf ts)
instance SymbolicInput (Product ts) => SymbolicInput (OneOf ts)
instance (Symbolic c, SymbolicEq (Product ts) c) => Eq (OneOf ts c)

embedOneOf :: forall ts c. (Embed ts, Symbolic c) => Eithers ts c -> OneOf ts c
embedOneOf =
  OneOf <$> fromConstant . indexOf @ts @c Proxy <*> embed @ts @c Proxy

matchOneOf
  :: forall r ts c
   . (SymbolicData r, Embed ts, Symbolic c)
  => OneOf ts c
  -> (Eithers ts c -> r c)
  -> r c
matchOneOf OneOf {..} f =
  case zipWith
    (\c b -> (fromConstant c, f b))
    [(0 :: Natural) ..]
    (enum @ts Proxy branches) of
    [] -> zeroed
    (b : bs) -> interpolate (b :| bs) (fromFieldElement discriminant)

------------------------------ Nested Product type family ----------------------

class SymbolicData (NP f) => Produces f c where
  type NP f :: Ctx -> Type
  produce :: f u -> NP f c
  utilize :: NP f c -> f u

instance (Produces f c, Produces g c) => Produces (f G.:*: g) c where
  type NP (f G.:*: g) = NP f G.:*: NP g
  produce (f G.:*: g) = produce f G.:*: produce g
  utilize (x G.:*: y) = utilize x G.:*: utilize y

instance Produces G.U1 c where
  type NP G.U1 = Proxy
  produce _ = Proxy
  utilize _ = G.U1

instance SymbolicData a => Produces (G.M1 G.S u (G.K1 v (a c))) c where
  type NP (G.M1 G.S u (G.K1 v (a c))) = a
  produce (G.M1 (G.K1 a)) = a
  utilize = G.M1 . G.K1

---------------- Sum-of-Products form of a Symbolic datatype -------------------

class Embed (SOP f '[]) => Injects f c where
  type SOP f (acc :: [Ctx -> Type]) :: [Ctx -> Type]
  sumFrom :: f u -> Proxy ts -> Eithers (SOP f ts) c
  sumSkip :: Proxy '(ts, f) -> Eithers ts c -> Eithers (SOP f ts) c
  sumTo :: Proxy ts -> Eithers (SOP f ts) c -> Either (f u) (Eithers ts c)

sopify :: forall c f u. Injects f c => f u -> Eithers (SOP f '[]) c
sopify f = sumFrom @_ @c f (Proxy @'[])

desop :: forall c f u. Injects f c => Eithers (SOP f '[]) c -> f u
desop = either id (\case) . sumTo (Proxy @'[])

instance
  (Injects f c, Injects g c, Embed (SOP f (SOP g '[])))
  => Injects (f G.:+: g) c
  where
  type SOP (f G.:+: g) acc = SOP f (SOP g acc)
  sumFrom (G.L1 f) (_ :: Proxy ts) = sumFrom f (Proxy @(SOP g ts))
  sumFrom (G.R1 g) (_ :: Proxy ts) =
    sumSkip (Proxy @'(SOP g ts, f)) $ sumFrom g (Proxy @ts)
  sumSkip (_ :: Proxy '(ts, p)) es =
    sumSkip (Proxy @'(SOP g ts, f)) $ sumSkip (Proxy @'(ts, g)) es
  sumTo (_ :: Proxy ts) =
    either (Left . G.L1) (first G.R1 . sumTo @g (Proxy @ts)) . sumTo @f (Proxy @(SOP g ts))

instance Injects G.V1 c where
  type SOP G.V1 acc = acc
  sumFrom x = case x of {}
  sumSkip _ es = es
  sumTo _ = Right

instance Injects f c => Injects (G.M1 G.D u f) c where
  type SOP (G.M1 G.D u f) acc = SOP f acc
  sumFrom (G.M1 f) = sumFrom f
  sumSkip (_ :: Proxy '(ts, g)) = sumSkip $ Proxy @'(ts, f)
  sumTo p = first G.M1 . sumTo @f p

instance Produces f c => Injects (G.M1 G.C u f) c where
  type SOP (G.M1 G.C u f) acc = NP f ': acc
  sumFrom (G.M1 f) _ = G.L1 (produce f)
  sumSkip _ = G.R1
  sumTo _ = \case
    G.L1 x -> Left $ G.M1 (utilize x)
    G.R1 x -> Right x

----------------------------- Sum wrapper datatype -----------------------------

type Cons a = SOP (G.Rep a) '[]
type Prod a = Product (Cons a)
newtype Sum a c = Sum {sumOf :: OneOf (Cons a) c}
deriving newtype instance SymbolicData (Prod a) => SymbolicData (Sum a)
deriving newtype instance SymbolicInput (Prod a) => SymbolicInput (Sum a)
deriving newtype instance (SymbolicEq (Prod a) c, Symbolic c) => Eq (Sum a c)

inject ::
  forall a c. (Generic a, Injects (G.Rep a) c, Symbolic c) => a -> Sum a c
inject = Sum . embedOneOf . sopify @c . G.from

match ::
  forall a r c.
  (SymbolicData r, Generic a, Injects (G.Rep a) c, Symbolic c) =>
  Sum a c -> (a -> r c) -> r c
match Sum {..} f = matchOneOf sumOf (f . G.to . desop)
