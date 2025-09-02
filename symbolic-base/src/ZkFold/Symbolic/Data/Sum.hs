{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module ZkFold.Symbolic.Data.Sum where

import Control.Applicative ((<*>))
import Data.Bifunctor (first)
import Data.Either (Either (..), either)
import Data.Function (const, id, ($), (.))
import Data.Functor (fmap, (<$>))
import Data.Kind (Type)
import Data.List (zipWith)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic, Generic1)
import qualified GHC.Generics as G
import Numeric.Natural (Natural)

import ZkFold.Algebra.Class
import ZkFold.Data.Eq (Eq)
import ZkFold.Symbolic.Class (Ctx, Symbolic)
import ZkFold.Symbolic.Data.Bool (SymbolicEq)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement (FieldElement, fromFieldElement)
import ZkFold.Symbolic.Data.Input (SymbolicInput)

------------------------------ Product & Eithers -------------------------------

type family Product ts where
  Product '[] = G.U1
  Product (t : ts) = t G.:*: Product ts

type family Eithers ts where
  Eithers '[] = G.V1
  Eithers (t : ts) = t G.:+: Eithers ts

class (SymbolicData (Product ts), HasRep (Product ts) c) => Embed ts c where
  embed :: Symbolic c => Proxy ts -> Eithers ts c -> Product ts c
  indexOf :: Proxy ts -> Eithers ts c -> Natural
  enum :: Proxy ts -> Product ts c -> [Eithers ts c]

instance Embed '[] c where
  embed _ = \case {}
  indexOf _ = \case {}
  enum _ = const []

instance (SymbolicData t, HasRep t c, Embed ts c) => Embed (t ': ts) c where
  embed _ = \case
    G.L1 x -> x G.:*: dummy
    G.R1 x -> dummy G.:*: embed @ts Proxy x
  indexOf _ = \case
    G.L1 _ -> zero
    G.R1 x -> one + indexOf @ts Proxy x
  enum _ (h G.:*: t) = G.L1 h : fmap G.R1 (enum @ts Proxy t)

--------------------------------- OneOf datatype -------------------------------

data OneOf ts c = OneOf
  { discriminant :: FieldElement c
  , branches :: Product ts c
  }
  deriving (Generic, Generic1)

instance SymbolicData (Product ts) => SymbolicData (OneOf ts)

instance SymbolicInput (Product ts) => SymbolicInput (OneOf ts)

instance (Symbolic c, SymbolicEq (Product ts) c) => Eq (OneOf ts c)

embedOneOf
  :: forall ts c. (Embed ts c, Symbolic c) => Eithers ts c -> OneOf ts c
embedOneOf = OneOf <$> fromConstant . indexOf @ts Proxy <*> embed @ts Proxy

matchOneOf
  :: forall r ts c
   . (Embed ts c, SymbolicData r, HasRep r c, Symbolic c)
  => OneOf ts c
  -> (Eithers ts c -> r c)
  -> r c
matchOneOf OneOf {..} f =
  case zipWith
    (\c b -> (fromConstant c, f b))
    [(0 :: Natural) ..]
    (enum @ts Proxy branches) of
    [] -> dummy
    (b : bs) -> interpolate (b :| bs) (fromFieldElement discriminant)

---------------- Sum-of-Products form of a Symbolic datatype -------------------

class Embed (SOP f '[]) c => Injects f c where
  type SOP f (acc :: [Ctx -> Type]) :: [Ctx -> Type]
  sumFrom :: f c -> Proxy ts -> Eithers (SOP f ts) c
  sumSkip :: Proxy '(ts, f) -> Eithers ts c -> Eithers (SOP f ts) c
  sumTo :: Proxy ts -> Eithers (SOP f ts) c -> Either (f c) (Eithers ts c)

sopify :: Injects f c => f c -> Eithers (SOP f '[]) c
sopify f = sumFrom f (Proxy @'[])

desop :: Injects f c => Eithers (SOP f '[]) c -> f c
desop = either id (\case {}) . sumTo (Proxy @'[])

instance
  ( Injects f c
  , Injects g c
  , Embed (SOP f (SOP g '[])) c
  , HasRep (Product (SOP f (SOP g '[]))) c
  )
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
  sumTo p = first G.M1 . sumTo p

instance (SymbolicData f, HasRep f c) => Injects (G.M1 G.C u f) c where
  type SOP (G.M1 G.C u f) acc = f ': acc
  sumFrom (G.M1 f) _ = G.L1 f
  sumSkip _ = G.R1
  sumTo _ = \case
    G.L1 x -> Left $ G.M1 x
    G.R1 x -> Right x

----------------------------- Sum wrapper datatype -----------------------------

type Cons a = SOP (G.Rep1 a) '[]

type Prod a = Product (Cons a)

newtype Sum a c = Sum {sumOf :: OneOf (Cons a) c}

deriving newtype instance SymbolicData (Prod a) => SymbolicData (Sum a)

deriving newtype instance SymbolicInput (Prod a) => SymbolicInput (Sum a)

deriving newtype instance (SymbolicEq (Prod a) c, Symbolic c) => Eq (Sum a c)

inject :: (Generic1 a, Injects (G.Rep1 a) c, Symbolic c) => a c -> Sum a c
inject = Sum . embedOneOf . sopify . G.from1

match
  :: (Generic1 a, SymbolicData r, HasRep r c, Injects (G.Rep1 a) c, Symbolic c)
  => Sum a c -> (a c -> r c) -> r c
match Sum {..} f = matchOneOf sumOf (f . G.to1 . desop)
