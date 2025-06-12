{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Class' (
        LayoutFunctor,
        SymbolicData (..),
        SymbolicFunction (..),
    ) where

import           Control.DeepSeq             (NFData1)
import           Data.Binary                 (Binary)
import           Data.Functor                (fmap)
import           Data.Functor.Rep            (Representable)
import qualified Data.Functor.Rep            as R
import           Data.Kind                   (Type)
import           Data.Traversable            (Traversable)
import           Data.Type.Equality          (type (~))
import           GHC.Generics                (U1 (..), (:*:) (..), (:.:) (..), V1)
import qualified GHC.Generics                as G

import           ZkFold.Algebra.Number       (KnownNat)
import           ZkFold.Control.HApplicative (hliftA2, hunit)
import           ZkFold.Data.ByteString      (Binary1)
import           ZkFold.Data.Orphans         ()
import           ZkFold.Data.Package         (pack)
import           ZkFold.Data.Vector          (Vector)
import           ZkFold.Symbolic.Class       (Symbolic (..))

type LayoutFunctor f = (Binary1 f, Binary (R.Rep f), NFData1 f, Representable f, Traversable f)

-- | A class for Symbolic data types.
class
    ( LayoutFunctor (Layout x)
    ) => SymbolicData x where

    type Layout x :: Type -> Type
    type Layout x = GLayout (G.Rep1 x)

    -- | Returns the circuit that makes up `x`.
    contextD :: Symbolic c => x c -> c (Layout x)
    default contextD
      :: ( G.Generic1 x
         , GSymbolicData (G.Rep1 x)
         , Layout x ~ GLayout (G.Rep1 x)
         )
      => Symbolic c => x c -> c (Layout x)
    contextD x = gcontextD (G.from1 x)

instance
    ( SymbolicData x
    , SymbolicData y
    ) => SymbolicData (x :*: y) where

instance (KnownNat n, SymbolicData x) => SymbolicData (Vector n :.: x)

class
    ( LayoutFunctor (GLayout x)
    ) => GSymbolicData x where

    type GLayout x :: Type -> Type

    gcontextD :: Symbolic c => x c -> c (GLayout x)

instance GSymbolicData (G.K1 i x) where
    type GLayout (G.K1 i x) = U1
    gcontextD (G.K1 _) = hunit

instance GSymbolicData f => GSymbolicData (G.M1 i c f) where
    type GLayout (G.M1 i c f) = GLayout f
    gcontextD (G.M1 a) = gcontextD a

instance
    ( GSymbolicData u
    , GSymbolicData v
    ) => GSymbolicData (u :*: v) where
    type GLayout (u :*: v) = GLayout u :*: GLayout v
    gcontextD (a :*: b) = hliftA2 (:*:) (gcontextD a) (gcontextD b)

instance (GSymbolicData x, LayoutFunctor f) => GSymbolicData (f :.: x) where
    type GLayout (f :.: x) = f :.: (GLayout x)
    gcontextD (G.Comp1 a) = pack (fmap gcontextD a)

instance SymbolicData x => GSymbolicData (G.Rec1 x) where
    type GLayout (G.Rec1 x) = Layout x
    gcontextD (G.Rec1 x) = contextD x

class SymbolicFunction f where
    type Context f :: (Type -> Type) -> Type
    type Support f :: Type
    type Image f   :: Type -> Type

    -- | Converts a function to a circuit.
    contextF :: f -> Support f -> Context f (Image f)

instance (SymbolicData x, Symbolic c) => SymbolicFunction (x c) where
    type Context (x c) = c
    type Support (x c) = V1 c
    type Image (x c) = Layout x
    contextF x _ = contextD x

instance (Symbolic c, SymbolicFunction y, Context y ~ c) => SymbolicFunction (x c -> y) where
    type Context (x c -> y) = c
    type Support (x c -> y) = (x c, Support y)
    type Image (x c -> y) = Image y
    contextF f (x, y) = contextF (f x) y
