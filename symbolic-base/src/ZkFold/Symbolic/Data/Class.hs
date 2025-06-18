{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module ZkFold.Symbolic.Data.Class (
        PayloadFunctor,
        LayoutFunctor,
        SymbolicDataConstraint,
        SymbolicData (..),
        SymbolicFunction (..),
        GSymbolicData (..),
    ) where

import           Control.DeepSeq             (NFData1)
import           Data.Binary                 (Binary)
import           Data.Functor                (fmap)
import           Data.Functor.Rep            (Representable)
import qualified Data.Functor.Rep            as R
import           Data.Kind                   (Type)
import           Data.Traversable            (Traversable)
import           Data.Type.Equality          (type (~))
import           GHC.Generics                (V1, (:*:) (..), (:.:) (..))
import qualified GHC.Generics                as G

import           ZkFold.Algebra.Class        (Finite)
import           ZkFold.Algebra.Number       (KnownNat)
import           ZkFold.Control.HApplicative (hpair)
import           ZkFold.Data.ByteString      (Binary1)
import           ZkFold.Data.HFunctor        (hmap)
import           ZkFold.Data.Orphans         ()
import           ZkFold.Data.Package         (pack, unpack)
import           ZkFold.Data.Product         (fstP, sndP)
import           ZkFold.Data.Vector          (Vector)
import           ZkFold.Symbolic.Class       (Symbolic (..))

type PayloadFunctor f = (Representable f, Traversable f, Binary (R.Rep f))

type LayoutFunctor f = (Binary1 f, NFData1 f, PayloadFunctor f)

class (PayloadFunctor (Layout x a)) => SymbolicDataConstraints x a
instance (PayloadFunctor (Layout x a)) => SymbolicDataConstraints x a

class (forall a . Finite a => SymbolicDataConstraints x a, SymbolicData x) => SymbolicDataConstraint x

-- | A class for Symbolic data types.
class SymbolicData x where

    type Layout x a :: Type -> Type
    type Layout x a = GLayout (G.Rep1 x) a

    -- | Returns the circuit that makes up `x`.
    toContext :: Symbolic c => x c -> c (Layout x (BaseField c))
    default toContext
      :: ( G.Generic1 x
         , GSymbolicData (G.Rep1 x)
         , GLayout (G.Rep1 x) (BaseField c) ~ Layout x (BaseField c)
         )
      => Symbolic c => x c -> c (Layout x (BaseField c))
    toContext x = gToContext (G.from1 x)

    fromContext :: Symbolic c => c (Layout x (BaseField c)) -> x c
    default fromContext
      :: ( G.Generic1 x
         , GSymbolicData (G.Rep1 x)
         , Layout x (BaseField c) ~ GLayout (G.Rep1 x) (BaseField c)
         )
      => Symbolic c => c (Layout x (BaseField c)) -> x c
    fromContext c = G.to1 (gFromContext c)

instance
    ( SymbolicData x
    , SymbolicData y
    ) => SymbolicData (x :*: y) where

instance (KnownNat n, SymbolicData x) => SymbolicData (Vector n :.: x)

class GSymbolicData x where

    type GLayout x a :: Type -> Type

    gToContext :: Symbolic c => x c -> c (GLayout x (BaseField c))
    gFromContext :: Symbolic c => c (GLayout x (BaseField c)) -> x c

instance GSymbolicData f => GSymbolicData (G.M1 i c f) where
    type GLayout (G.M1 i c f) n = GLayout f n
    gToContext (G.M1 a) = gToContext a
    gFromContext c = G.M1 (gFromContext c)

instance
    ( GSymbolicData u
    , GSymbolicData v
    ) => GSymbolicData (u :*: v) where
    type GLayout (u :*: v) n = GLayout u n :*: GLayout v n
    gToContext (a :*: b) = hpair (gToContext a) (gToContext b)
    gFromContext c = gFromContext (hmap fstP c) :*: gFromContext (hmap sndP c)

instance (GSymbolicData x, LayoutFunctor f) => GSymbolicData (f :.: x) where
    type GLayout (f :.: x) n = f :.: (GLayout x n)
    gToContext (G.Comp1 a) = pack (fmap gToContext a)
    gFromContext c = Comp1 (fmap gFromContext (unpack c))

instance SymbolicData x => GSymbolicData (G.Rec1 x) where
    type GLayout (G.Rec1 x) n = Layout x n
    gToContext (G.Rec1 x) = toContext x
    gFromContext c = G.Rec1 (fromContext c)

class SymbolicFunction f where
    type Context f :: (Type -> Type) -> Type
    type Support f :: Type
    type Image f   :: Type -> Type

    -- | Converts a function to a circuit.
    arithmetize :: f -> Support f -> Context f (Image f)

instance (SymbolicData x, Symbolic c) => SymbolicFunction (x c) where
    type Context (x c) = c
    type Support (x c) = V1 c
    type Image (x c) = Layout x (BaseField c)
    arithmetize x _ = toContext x

instance (Symbolic c, SymbolicFunction y, Context y ~ c) => SymbolicFunction (x c -> y) where
    type Context (x c -> y) = c
    type Support (x c -> y) = (x c, Support y)
    type Image (x c -> y) = Image y
    arithmetize f (x, y) = arithmetize (f x) y
