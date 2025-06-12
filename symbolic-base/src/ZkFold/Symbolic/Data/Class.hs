{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Class (
        LayoutFunctor,
        PayloadFunctor,
        SymbolicData (..),
        SymbolicOutput,
        LayoutData (..),
        GSymbolicData (..),
        WitnessContext (..),
    ) where

import           Control.Applicative         ((<*>))
import           Control.DeepSeq             (NFData1)
import           Data.Bifunctor              (bimap)
import           Data.Binary                 (Binary)
import           Data.Function               (flip, ($), (.))
import           Data.Functor                (fmap, (<$>))
import           Data.Functor.Rep            (Representable)
import qualified Data.Functor.Rep            as R
import           Data.Kind                   (Type)
import           Data.Traversable            (Traversable)
import           Data.Tuple                  (fst)
import           Data.Type.Equality          (type (~))
import           Data.Typeable               (Proxy (..))
import           GHC.Generics                (U1 (..), (:*:) (..), (:.:) (..))
import qualified GHC.Generics                as G

import           ZkFold.Algebra.Number       (KnownNat)
import           ZkFold.Control.HApplicative (hliftA2, hpure)
import           ZkFold.Data.ByteString      (Binary1)
import           ZkFold.Data.HFunctor        (hmap)
import           ZkFold.Data.Orphans         ()
import           ZkFold.Data.Package         (pack)
import           ZkFold.Data.Product         (fstP, sndP)
import           ZkFold.Data.Vector          (Vector)
import           ZkFold.Symbolic.Class       (Symbolic (..), embedW)

type PayloadFunctor f = (Representable f, Binary (R.Rep f))

type LayoutFunctor f = (Binary1 f, NFData1 f, PayloadFunctor f, Traversable f)

newtype WitnessContext c f = WitnessContext
    { fromWitnessContext :: f (WitnessField c)
    }

-- | A class for Symbolic data types.
class
    ( Symbolic (Context x)
    , LayoutFunctor (Layout x)
    , PayloadFunctor (Payload x)
    ) => SymbolicData x where

    type Witness x :: Type
    type Witness x = GWitness (G.Rep x)

    type Context x :: (Type -> Type) -> Type
    type Context x = GContext (G.Rep x)

    type Support x :: Type
    type Support x = GSupport (G.Rep x)

    type Layout x :: Type -> Type
    type Layout x = GLayout (G.Rep x)

    type Payload x :: Type -> Type
    type Payload x = GPayload (G.Rep x)

    fromWitness :: Witness x -> x
    default fromWitness
      :: ( G.Generic x
         , GSymbolicData (G.Rep x)
         , Witness x ~ GWitness (G.Rep x)
         ) =>
      Witness x -> x
    fromWitness w = G.to (gFromWitness w)

    toWitness :: x -> Witness x
    default toWitness
      :: ( G.Generic x
         , GSymbolicData (G.Rep x)
         , Witness x ~ GWitness (G.Rep x)
         ) =>
      x -> Witness x
    toWitness x = gtoWitness (G.from x)

    -- | Returns the circuit that makes up `x`.
    arithmetize :: x -> Support x -> Context x (Layout x)
    default arithmetize
      :: ( G.Generic x
         , GSymbolicData (G.Rep x)
         , Context x ~ GContext (G.Rep x)
         , Support x ~ GSupport (G.Rep x)
         , Layout x ~ GLayout (G.Rep x)
         )
      => x -> Support x -> Context x (Layout x)
    arithmetize x = garithmetize (G.from x)

    payload :: x -> Support x -> Payload x (WitnessField (Context x))
    default payload
      :: ( G.Generic x
         , GSymbolicData (G.Rep x)
         , Context x ~ GContext (G.Rep x)
         , Support x ~ GSupport (G.Rep x)
         , Payload x ~ GPayload (G.Rep x)
         )
      => x -> Support x -> Payload x (WitnessField (Context x))
    payload x = gpayload (G.from x)

    -- | Restores `x` from the circuit's outputs.
    restore ::
      Context x ~ c =>
      (Support x -> (c (Layout x), Payload x (WitnessField c))) -> x
    default restore ::
      ( Context x ~ c, G.Generic x, GSymbolicData (G.Rep x)
      , Context x ~ GContext (G.Rep x)
      , Support x ~ GSupport (G.Rep x)
      , Layout x ~ GLayout (G.Rep x)
      , Payload x ~ GPayload (G.Rep x)) =>
      (Support x -> (c (Layout x), Payload x (WitnessField c))) -> x
    restore f = G.to (grestore f)

type SymbolicOutput x = (SymbolicData x, Support x ~ Proxy (Context x))

instance (Symbolic c, LayoutFunctor f) => SymbolicData (c f) where
    type Witness (c f) = f (WitnessField c)
    type Context (c f) = c
    type Support (c f) = Proxy c
    type Layout (c f) = f
    type Payload (c f) = U1

    fromWitness = embedW
    toWitness = witnessF
    arithmetize x _ = x
    payload _ _ = U1
    restore f = fst (f Proxy)

instance Symbolic c => SymbolicData (Proxy (c :: (Type -> Type) -> Type)) where
    type Witness (Proxy c) = U1 (WitnessField c)
    type Context (Proxy c) = c
    type Support (Proxy c) = Proxy c
    type Layout (Proxy c) = U1
    type Payload (Proxy c) = U1

    fromWitness _ = Proxy
    toWitness _ = U1
    arithmetize _ _ = hpure U1
    payload _ _ = U1
    restore _ = Proxy

instance
    ( SymbolicData x
    , SymbolicData y
    , Context x ~ Context y
    , Support x ~ Support y
    ) => SymbolicData (x, y) where

instance
    ( SymbolicData x
    , SymbolicData y
    , SymbolicData z
    , Context x ~ Context y
    , Context y ~ Context z
    , Support x ~ Support y
    , Support y ~ Support z
    ) => SymbolicData (x, y, z) where

instance
    ( SymbolicData w
    , SymbolicData x
    , SymbolicData y
    , SymbolicData z
    , Context w ~ Context x
    , Context x ~ Context y
    , Context y ~ Context z
    , Support w ~ Support x
    , Support x ~ Support y
    , Support y ~ Support z
    ) => SymbolicData (w, x, y, z) where

instance
    ( SymbolicData v
    , SymbolicData w
    , SymbolicData x
    , SymbolicData y
    , SymbolicData z
    , Context v ~ Context w
    , Context w ~ Context x
    , Context x ~ Context y
    , Context y ~ Context z
    , Support v ~ Support w
    , Support w ~ Support x
    , Support x ~ Support y
    , Support y ~ Support z
    ) => SymbolicData (v, w, x, y, z) where

instance
    ( SymbolicData u
    , SymbolicData v
    , SymbolicData w
    , SymbolicData x
    , SymbolicData y
    , SymbolicData z
    , Context u ~ Context v
    , Context v ~ Context w
    , Context w ~ Context x
    , Context x ~ Context y
    , Context y ~ Context z
    , Support u ~ Support v
    , Support v ~ Support w
    , Support w ~ Support x
    , Support x ~ Support y
    , Support y ~ Support z
    ) => SymbolicData (u, v, w, x, y, z) where

instance
    ( SymbolicData t
    , SymbolicData u
    , SymbolicData v
    , SymbolicData w
    , SymbolicData x
    , SymbolicData y
    , SymbolicData z
    , Context t ~ Context u
    , Context u ~ Context v
    , Context v ~ Context w
    , Context w ~ Context x
    , Context x ~ Context y
    , Context y ~ Context z
    , Support t ~ Support u
    , Support u ~ Support v
    , Support v ~ Support w
    , Support w ~ Support x
    , Support x ~ Support y
    , Support y ~ Support z
    ) => SymbolicData (t, u, v, w, x, y, z) where

newtype LayoutData f x = LayoutData { layoutData :: f x }

instance
    (LayoutFunctor f, SymbolicData x) =>
    SymbolicData (LayoutData f x) where
    type Witness (LayoutData f x) = f (Witness x)
    type Context (LayoutData f x) = Context x
    type Support (LayoutData f x) = Support x
    type Layout (LayoutData f x) = f :.: Layout x
    type Payload (LayoutData f x) = f :.: Payload x

    fromWitness = LayoutData . fmap fromWitness
    toWitness (LayoutData xs) = fmap toWitness xs
    arithmetize (LayoutData xs) s = pack (flip arithmetize s <$> xs)
    payload (LayoutData xs) s = Comp1 (flip payload s <$> xs)
    restore f = LayoutData . R.tabulate $ restore . (. f) . \i ->
        bimap (hmap (ix i)) (ix i)
        where ix i = flip R.index i . unComp1

deriving via (LayoutData (Vector n) x)
    instance (SymbolicData x, KnownNat n) => SymbolicData (Vector n x)

instance (SymbolicData x, SymbolicData y) => SymbolicData (x -> y) where
    type Witness (x -> y) = Witness x -> Witness y
    type Context (x -> y) = Context y
    type Support (x -> y) = (x, Support y)
    type Layout (x -> y) = Layout y
    type Payload (x -> y) = Payload y

    fromWitness f x = fromWitness (f (toWitness x))
    toWitness f x = toWitness (f (fromWitness x))
    arithmetize f (x, i) = arithmetize (f x) i
    payload f (x, i) = payload (f x) i
    restore f x = restore (f . (x,))

class
    ( Symbolic (GContext u)
    , LayoutFunctor (GLayout u)
    , PayloadFunctor (GPayload u)
    ) => GSymbolicData u where

    type GWitness u :: Type
    type GContext u :: (Type -> Type) -> Type
    type GSupport u :: Type
    type GLayout u :: Type -> Type
    type GPayload u :: Type -> Type

    gFromWitness :: GWitness u -> u x
    gtoWitness :: u x -> GWitness u
    garithmetize :: u x -> GSupport u -> GContext u (GLayout u)
    gpayload :: u x -> GSupport u -> GPayload u (WitnessField (GContext u))
    grestore ::
      GContext u ~ c =>
      (GSupport u -> (c (GLayout u), GPayload u (WitnessField c))) -> u x

instance
    ( GSymbolicData u
    , GSymbolicData v
    , GContext u ~ GContext v
    , GSupport u ~ GSupport v
    ) => GSymbolicData (u :*: v) where

    type GWitness (u :*: v) = (GWitness u, GWitness v)
    type GContext (u :*: v) = GContext u
    type GSupport (u :*: v) = GSupport u
    type GLayout (u :*: v) = GLayout u :*: GLayout v
    type GPayload (u :*: v) = GPayload u :*: GPayload v

    gFromWitness (a, b) = gFromWitness a :*: gFromWitness b
    gtoWitness (a :*: b) = (gtoWitness a, gtoWitness b)
    garithmetize (a :*: b) = hliftA2 (:*:) <$> garithmetize a <*> garithmetize b
    gpayload (a :*: b) = (:*:) <$> gpayload a <*> gpayload b
    grestore f =
      grestore (bimap (hmap fstP) fstP . f)
      :*: grestore (bimap (hmap sndP) sndP . f)

instance GSymbolicData f => GSymbolicData (G.M1 i c f) where

    type GWitness (G.M1 i c f) = GWitness f
    type GContext (G.M1 i c f) = GContext f
    type GSupport (G.M1 i c f) = GSupport f
    type GLayout (G.M1 i c f) = GLayout f
    type GPayload (G.M1 i c f) = GPayload f

    gFromWitness x = G.M1 (gFromWitness x)
    gtoWitness (G.M1 x) = gtoWitness x
    garithmetize (G.M1 a) = garithmetize a
    gpayload (G.M1 a) = gpayload a
    grestore f = G.M1 (grestore f)

instance SymbolicData x => GSymbolicData (G.Rec0 x) where

    type GWitness (G.Rec0 x) = Witness x
    type GContext (G.Rec0 x) = Context x
    type GSupport (G.Rec0 x) = Support x
    type GLayout (G.Rec0 x) = Layout x
    type GPayload (G.Rec0 x) = Payload x

    gFromWitness x = G.K1 (fromWitness x)
    gtoWitness (G.K1 x) = toWitness x
    garithmetize (G.K1 x) = arithmetize x
    gpayload (G.K1 x) = payload x
    grestore f = G.K1 (restore f)
