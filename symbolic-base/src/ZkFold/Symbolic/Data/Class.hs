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
    ) where

import           Control.Applicative                     (liftA2, (<*>))
import           Control.DeepSeq                         (NFData (..), NFData1, liftRnf)
import           Data.Bifunctor                          (bimap)
import           Data.Binary                             (Binary)
import           Data.Eq                                 (Eq)
import           Data.Foldable                           (Foldable)
import           Data.Function                           (const, flip, ($), (.))
import           Data.Functor                            (Functor, fmap, (<$>))
import           Data.Functor.Rep                        (Representable)
import qualified Data.Functor.Rep                        as R
import           Data.Kind                               (Type)
import           Data.List.NonEmpty                      (NonEmpty)
import           Data.Traversable                        (Traversable)
import           Data.Tuple                              (fst)
import           Data.Type.Equality                      (type (~))
import           Data.Typeable                           (Proxy (..))
import           GHC.Generics                            (U1 (..), (:*:) (..), (:.:) (..))
import qualified GHC.Generics                            as G
import           Text.Show                               (Show)

import           ZkFold.Algebra.Number                   (KnownNat)
import           ZkFold.Control.HApplicative             (hliftA2, hpure)
import           ZkFold.Data.ByteString                  (Binary1)
import           ZkFold.Data.HFunctor                    (hmap)
import           ZkFold.Data.Orphans                     ()
import           ZkFold.Data.Package                     (pack)
import           ZkFold.Data.Product                     (fstP, sndP)
import           ZkFold.Data.Vector                      (Vector)
import           ZkFold.Symbolic.Algorithm.Interpolation (interpolation)
import           ZkFold.Symbolic.Class                   (BaseField, Symbolic, WitnessField)

type PayloadFunctor f = (Representable f, Binary (R.Rep f))

type LayoutFunctor f = (Binary1 f, NFData1 f, PayloadFunctor f, Traversable f)

-- | A class for Symbolic data types.
class
    ( Symbolic (Context x)
    , LayoutFunctor (Layout x)
    , PayloadFunctor (Payload x)
    ) => SymbolicData x where

    type Context x :: (Type -> Type) -> Type
    type Context x = GContext (G.Rep x)

    type Support x :: Type
    type Support x = GSupport (G.Rep x)

    type Layout x :: Type -> Type
    type Layout x = GLayout (G.Rep x)

    type Payload x :: Type -> Type
    type Payload x = GPayload (G.Rep x)

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
    arithmetize = garithmetize . G.from

    payload :: x -> Support x -> Payload x (WitnessField (Context x))
    default payload
      :: ( G.Generic x
         , GSymbolicData (G.Rep x)
         , Context x ~ GContext (G.Rep x)
         , Support x ~ GSupport (G.Rep x)
         , Payload x ~ GPayload (G.Rep x)
         )
      => x -> Support x -> Payload x (WitnessField (Context x))
    payload = gpayload . G.from

    -- | Interpolates branch values between given points.
    interpolate :: Context x ~ c => NonEmpty (BaseField c, x) -> c G.Par1 -> x
    default interpolate
        :: ( Context x ~ c, G.Generic x
           , GSymbolicData (G.Rep x)
           , Context x ~ GContext (G.Rep x)
           )
        => NonEmpty (BaseField c, x) -> c G.Par1 -> x
    interpolate = (G.to .) . ginterpolate . fmap (G.from <$>)

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
    restore = G.to . grestore

type SymbolicOutput x = (SymbolicData x, Support x ~ Proxy (Context x))

instance (Symbolic c, LayoutFunctor f) => SymbolicData (c f) where
    type Context (c f) = c
    type Support (c f) = Proxy c
    type Layout (c f) = f
    type Payload (c f) = U1

    arithmetize = const
    payload _ _ = U1
    interpolate = interpolation
    restore = fst . ($ Proxy)

instance Symbolic c => SymbolicData (Proxy (c :: (Type -> Type) -> Type)) where
    type Context (Proxy c) = c
    type Support (Proxy c) = Proxy c
    type Layout (Proxy c) = U1
    type Payload (Proxy c) = U1

    arithmetize _ _ = hpure U1
    payload _ _ = U1
    interpolate _ _ = Proxy
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
    deriving newtype (Show, Eq, Functor, Foldable, NFData1)

instance (NFData1 f, NFData x) => NFData (LayoutData f x) where
    rnf = liftRnf rnf

instance
    (LayoutFunctor f, SymbolicData x) =>
    SymbolicData (LayoutData f x) where

    type Context (LayoutData f x) = Context x
    type Support (LayoutData f x) = Support x
    type Layout (LayoutData f x) = f :.: Layout x
    type Payload (LayoutData f x) = f :.: Payload x

    arithmetize (LayoutData xs) s = pack (flip arithmetize s <$> xs)
    payload (LayoutData xs) s = Comp1 (flip payload s <$> xs)
    interpolate bs = LayoutData . R.tabulate . flip \i ->
        interpolate (fmap (flip R.index i . layoutData) <$> bs)
    restore f = LayoutData . R.tabulate $ restore . (. f) . \i ->
        bimap (hmap (ix i)) (ix i)
        where ix i = flip R.index i . unComp1

deriving via (LayoutData (Vector n) x)
    instance (SymbolicData x, KnownNat n) => SymbolicData (Vector n x)

instance SymbolicData f => SymbolicData (x -> f) where
    type Context (x -> f) = Context f
    type Support (x -> f) = (x, Support f)
    type Layout (x -> f) = Layout f
    type Payload (x -> f) = Payload f

    arithmetize f (x, i) = arithmetize (f x) i
    payload f (x, i) = payload (f x) i
    interpolate bs p x = interpolate (fmap ($ x) <$> bs) p
    restore f x = restore (f . (x,))

class
    ( Symbolic (GContext u)
    , LayoutFunctor (GLayout u)
    , PayloadFunctor (GPayload u)
    ) => GSymbolicData u where

    type GContext u :: (Type -> Type) -> Type
    type GSupport u :: Type
    type GLayout u :: Type -> Type
    type GPayload u :: Type -> Type

    garithmetize :: u x -> GSupport u -> GContext u (GLayout u)
    gpayload :: u x -> GSupport u -> GPayload u (WitnessField (GContext u))
    ginterpolate ::
        GContext u ~ c => NonEmpty (BaseField c, u x) -> c G.Par1 -> u x
    grestore ::
        GContext u ~ c =>
        (GSupport u -> (c (GLayout u), GPayload u (WitnessField c))) -> u x

instance
    ( GSymbolicData u
    , GSymbolicData v
    , GContext u ~ GContext v
    , GSupport u ~ GSupport v
    ) => GSymbolicData (u :*: v) where

    type GContext (u :*: v) = GContext u
    type GSupport (u :*: v) = GSupport u
    type GLayout (u :*: v) = GLayout u :*: GLayout v
    type GPayload (u :*: v) = GPayload u :*: GPayload v

    garithmetize (a :*: b) = hliftA2 (:*:) <$> garithmetize a <*> garithmetize b
    gpayload (a :*: b) = (:*:) <$> gpayload a <*> gpayload b
    ginterpolate bs =
        liftA2 (:*:)
            (ginterpolate (fmap fstP <$> bs))
            (ginterpolate (fmap sndP <$> bs))
    grestore f =
        grestore (bimap (hmap fstP) fstP . f)
        :*: grestore (bimap (hmap sndP) sndP . f)

instance GSymbolicData f => GSymbolicData (G.M1 i c f) where
    type GContext (G.M1 i c f) = GContext f
    type GSupport (G.M1 i c f) = GSupport f
    type GLayout (G.M1 i c f) = GLayout f
    type GPayload (G.M1 i c f) = GPayload f
    garithmetize = garithmetize . G.unM1
    gpayload = gpayload . G.unM1
    ginterpolate = (G.M1 .) . ginterpolate . fmap (G.unM1 <$>)
    grestore = G.M1 . grestore

instance SymbolicData x => GSymbolicData (G.Rec0 x) where
    type GContext (G.Rec0 x) = Context x
    type GSupport (G.Rec0 x) = Support x
    type GLayout (G.Rec0 x) = Layout x
    type GPayload (G.Rec0 x) = Payload x
    garithmetize = arithmetize . G.unK1
    gpayload = payload . G.unK1
    ginterpolate = (G.K1 .) . interpolate . fmap (G.unK1 <$>)
    grestore = G.K1 . restore
