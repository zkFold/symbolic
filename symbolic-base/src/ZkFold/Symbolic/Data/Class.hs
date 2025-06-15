{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Class (
        PayloadFunctor,
        LayoutFunctor,
        SymbolicData (..),
        SymbolicFunction (..),
        Sym (..),
        Wit (..),
        GSymbolicData (..),
    ) where

import           Control.DeepSeq             (NFData1, NFData)
import           Data.Binary                 (Binary)
import           Data.Functor                (fmap)
import           Data.Functor.Rep            (Representable)
import qualified Data.Functor.Rep            as R
import           Data.Kind                   (Type)
import           Data.Traversable            (Traversable)
import           Data.Type.Equality          (type (~))
import           GHC.Generics                (V1, (:*:) (..), (:.:) (..))
import qualified GHC.Generics                as G

import           ZkFold.Algebra.Number       (KnownNat)
import           ZkFold.Control.HApplicative (hpair)
import           ZkFold.Data.ByteString      (Binary1)
import           ZkFold.Data.HFunctor        (hmap)
import           ZkFold.Data.Orphans         ()
import           ZkFold.Data.Package         (pack, unpack)
import           ZkFold.Data.Product         (fstP, sndP)
import           ZkFold.Data.Vector          (Vector)
import           ZkFold.Symbolic.Class       (Symbolic (..), embedW)
import Data.Function (id, (.))
import ZkFold.Data.HFunctor.Classes (HNFData, HEq, HShow)
import Data.Functor.Classes (Eq1, Show1)
import Data.Eq (Eq)
import GHC.Show (Show)

-- | A newtype wrapper for deriving `Generic1` for symbolic data types.
newtype Sym f c = Sym { runSym :: c f }

deriving instance (HNFData c, NFData1 f) => NFData (Sym f c)
deriving instance (HEq c, Eq1 f) => Eq (Sym f c)
deriving instance (HShow c, Show1 f) => Show (Sym f c)

instance G.Generic1 (Sym f) where
    type Rep1 (Sym f) = Sym f
    from1 = id
    to1 = id

newtype Wit f c = Wit (f (WitnessField c))

instance G.Generic1 (Wit f) where
    type Rep1 (Wit f) = Wit f
    from1 = id
    to1 = id

type PayloadFunctor f = (Representable f, Traversable f, Binary (R.Rep f))

type LayoutFunctor f = (Binary1 f, NFData1 f, PayloadFunctor f)

-- | A class for Symbolic data types.
class
    ( PayloadFunctor (Layout x)
    ) => SymbolicData x where

    type Layout x :: Type -> Type
    type Layout x = GLayout (G.Rep1 x)

    -- | Returns the circuit that makes up `x`.
    toContext :: Symbolic c => x c -> c (Layout x)
    default toContext
      :: ( G.Generic1 x
         , GSymbolicData (G.Rep1 x)
         , GLayout (G.Rep1 x) ~ Layout x
         )
      => Symbolic c => x c -> c (Layout x)
    toContext x = gToContext (G.from1 x)

    fromContext :: Symbolic c => c (Layout x) -> x c
    default fromContext
      :: ( G.Generic1 x
         , GSymbolicData (G.Rep1 x)
         , Layout x ~ GLayout (G.Rep1 x)
         )
      => Symbolic c => c (Layout x) -> x c
    fromContext c = G.to1 (gFromContext c)

instance LayoutFunctor f => SymbolicData (Sym f)

instance PayloadFunctor f => SymbolicData (Wit f)

instance
    ( SymbolicData x
    , SymbolicData y
    ) => SymbolicData (x :*: y) where

instance (KnownNat n, SymbolicData x) => SymbolicData (Vector n :.: x)

class
    ( PayloadFunctor (GLayout x)
    ) => GSymbolicData x where

    type GLayout x :: Type -> Type

    gToContext :: Symbolic c => x c -> c (GLayout x)
    gFromContext :: Symbolic c => c (GLayout x) -> x c

instance GSymbolicData f => GSymbolicData (G.M1 i c f) where
    type GLayout (G.M1 i c f) = GLayout f
    gToContext (G.M1 a) = gToContext a
    gFromContext c = G.M1 (gFromContext c)

instance
    ( GSymbolicData u
    , GSymbolicData v
    ) => GSymbolicData (u :*: v) where
    type GLayout (u :*: v) = GLayout u :*: GLayout v
    gToContext (a :*: b) = hpair (gToContext a) (gToContext b)
    gFromContext c = gFromContext (hmap fstP c) :*: gFromContext (hmap sndP c)

instance (GSymbolicData x, LayoutFunctor f) => GSymbolicData (f :.: x) where
    type GLayout (f :.: x) = f :.: (GLayout x)
    gToContext (G.Comp1 a) = pack (fmap gToContext a)
    gFromContext c = Comp1 (fmap gFromContext (unpack c))

instance SymbolicData x => GSymbolicData (G.Rec1 x) where
    type GLayout (G.Rec1 x) = Layout x
    gToContext (G.Rec1 x) = toContext x
    gFromContext c = G.Rec1 (fromContext c)

instance LayoutFunctor f =>  GSymbolicData (Sym f) where
    type GLayout (Sym f) = f
    gToContext (Sym x) = x
    gFromContext x = Sym x

instance PayloadFunctor f => GSymbolicData (Wit f) where
    type GLayout (Wit f) = f
    gToContext (Wit w) = embedW w
    gFromContext = Wit . witnessF

class PayloadFunctor (Image f) => SymbolicFunction f where
    type Context f :: (Type -> Type) -> Type
    type Support f :: Type
    type Image f   :: Type -> Type

    -- | Converts a function to a circuit.
    arithmetize :: f -> Support f -> Context f (Image f)

instance (SymbolicData x, Symbolic c) => SymbolicFunction (x c) where
    type Context (x c) = c
    type Support (x c) = V1 c
    type Image (x c) = Layout x
    arithmetize x _ = toContext x

instance (Symbolic c, SymbolicFunction y, Context y ~ c) => SymbolicFunction (x c -> y) where
    type Context (x c -> y) = c
    type Support (x c -> y) = (x c, Support y)
    type Image (x c -> y) = Image y
    arithmetize f (x, y) = arithmetize (f x) y
