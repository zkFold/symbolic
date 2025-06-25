{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Class (
  LayoutFunctor,
  PayloadFunctor,
  SymbolicData (..),
  SymbolicFunction (..),
  Domain,
  Range,
  LayoutData (..),
  GSymbolicData (..),
) where

import Control.DeepSeq (NFData (..), NFData1, liftRnf)
import Data.Bifunctor (bimap)
import Data.Binary (Binary)
import Data.Eq (Eq)
import Data.Foldable (Foldable)
import Data.Function (($))
import Data.Functor (Functor, (<$>))
import Data.Functor.Rep (Representable, mzipWithRep)
import qualified Data.Functor.Rep as R
import Data.Kind (Type)
import Data.Traversable (Traversable)
import Data.Tuple (curry, fst)
import Data.Type.Equality (type (~))
import Data.Typeable (Proxy (..))
import GHC.Generics (U1 (..), (:*:) (..), (:.:) (..))
import qualified GHC.Generics as G
import Text.Show (Show)
import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Control.HApplicative (hliftA2, hpure)
import ZkFold.Data.ByteString (Binary1)
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.Orphans ()
import ZkFold.Data.Package (pack, unpack)
import ZkFold.Data.Product (fstP, sndP)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Symbolic (WitnessField))

type PayloadFunctor f = (Representable f, Binary (R.Rep f))

type LayoutFunctor f = (Binary1 f, NFData1 f, PayloadFunctor f, Traversable f)

-- | A class for Symbolic data types.
class
  ( Symbolic (Context x)
  , LayoutFunctor (Layout x)
  , PayloadFunctor (Payload x)
  , Range x ~ x
  , Domain x ~ Proxy (Context x)
  ) =>
  SymbolicData x
  where
  type Context x :: (Type -> Type) -> Type
  type Context x = GContext (G.Rep x)

  type Layout x :: Type -> Type
  type Layout x = GLayout (G.Rep x)

  type Payload x :: Type -> Type
  type Payload x = GPayload (G.Rep x)

  -- | Returns the circuit that makes up `x`.
  arithmetize :: x -> Context x (Layout x)
  default arithmetize
    :: ( G.Generic x
       , GSymbolicData (G.Rep x)
       , Context x ~ GContext (G.Rep x)
       , Layout x ~ GLayout (G.Rep x)
       )
    => x
    -> Context x (Layout x)
  arithmetize x = garithmetize (G.from x)

  payload :: x -> Payload x (WitnessField (Context x))
  default payload
    :: ( G.Generic x
       , GSymbolicData (G.Rep x)
       , Context x ~ GContext (G.Rep x)
       , Payload x ~ GPayload (G.Rep x)
       )
    => x
    -> Payload x (WitnessField (Context x))
  payload x = gpayload (G.from x)

  -- | Restores `x` from the circuit's outputs.
  restore
    :: Context x ~ c
    => (c (Layout x), Payload x (WitnessField c))
    -> x
  default restore
    :: ( Context x ~ c
       , G.Generic x
       , GSymbolicData (G.Rep x)
       , Context x ~ GContext (G.Rep x)
       , Layout x ~ GLayout (G.Rep x)
       , Payload x ~ GPayload (G.Rep x)
       )
    => (c (Layout x), Payload x (WitnessField c))
    -> x
  restore f = G.to (grestore f)

instance (Symbolic c, LayoutFunctor f) => SymbolicData (c f) where
  type Context (c f) = c
  type Layout (c f) = f
  type Payload (c f) = U1

  arithmetize x = x
  payload _ = U1
  restore = fst

instance Symbolic c => SymbolicData (Proxy (c :: (Type -> Type) -> Type)) where
  type Context (Proxy c) = c
  type Layout (Proxy c) = U1
  type Payload (Proxy c) = U1

  arithmetize _ = hpure U1
  payload _ = U1
  restore _ = Proxy

instance
  ( SymbolicData x
  , SymbolicData y
  , Context x ~ Context y
  )
  => SymbolicData (x, y)

instance
  ( SymbolicData x
  , SymbolicData y
  , SymbolicData z
  , Context x ~ Context y
  , Context y ~ Context z
  )
  => SymbolicData (x, y, z)

instance
  ( SymbolicData w
  , SymbolicData x
  , SymbolicData y
  , SymbolicData z
  , Context w ~ Context x
  , Context x ~ Context y
  , Context y ~ Context z
  )
  => SymbolicData (w, x, y, z)

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
  )
  => SymbolicData (v, w, x, y, z)

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
  )
  => SymbolicData (u, v, w, x, y, z)

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
  )
  => SymbolicData (t, u, v, w, x, y, z)

newtype LayoutData f x = LayoutData {layoutData :: f x}
  deriving newtype (Eq, Foldable, Functor, NFData1, Show)

instance (NFData1 f, NFData x) => NFData (LayoutData f x) where
  rnf = liftRnf rnf

instance
  (LayoutFunctor f, SymbolicData x)
  => SymbolicData (LayoutData f x)
  where
  type Context (LayoutData f x) = Context x
  type Layout (LayoutData f x) = f :.: Layout x
  type Payload (LayoutData f x) = f :.: Payload x

  arithmetize (LayoutData xs) = pack (arithmetize <$> xs)
  payload (LayoutData xs) = Comp1 (payload <$> xs)
  restore (c, Comp1 ps) = LayoutData $ mzipWithRep (curry restore) (unpack c) ps

deriving via
  (LayoutData (Vector n) x)
  instance
    (SymbolicData x, KnownNat n) => SymbolicData (Vector n x)

class
  ( Symbolic (GContext u)
  , LayoutFunctor (GLayout u)
  , PayloadFunctor (GPayload u)
  ) =>
  GSymbolicData u
  where
  type GContext u :: (Type -> Type) -> Type
  type GLayout u :: Type -> Type
  type GPayload u :: Type -> Type

  garithmetize :: u x -> GContext u (GLayout u)
  gpayload :: u x -> GPayload u (WitnessField (GContext u))
  grestore
    :: GContext u ~ c
    => (c (GLayout u), GPayload u (WitnessField c))
    -> u x

instance
  ( GSymbolicData u
  , GSymbolicData v
  , GContext u ~ GContext v
  )
  => GSymbolicData (u :*: v)
  where
  type GContext (u :*: v) = GContext u
  type GLayout (u :*: v) = GLayout u :*: GLayout v
  type GPayload (u :*: v) = GPayload u :*: GPayload v

  garithmetize (a :*: b) = hliftA2 (:*:) (garithmetize a) (garithmetize b)
  gpayload (a :*: b) = gpayload a :*: gpayload b
  grestore f =
    grestore (bimap (hmap fstP) fstP f)
      :*: grestore (bimap (hmap sndP) sndP f)

instance GSymbolicData f => GSymbolicData (G.M1 i c f) where
  type GContext (G.M1 i c f) = GContext f
  type GLayout (G.M1 i c f) = GLayout f
  type GPayload (G.M1 i c f) = GPayload f
  garithmetize (G.M1 a) = garithmetize a
  gpayload (G.M1 a) = gpayload a
  grestore f = G.M1 (grestore f)

instance SymbolicData x => GSymbolicData (G.Rec0 x) where
  type GContext (G.Rec0 x) = Context x
  type GLayout (G.Rec0 x) = Layout x
  type GPayload (G.Rec0 x) = Payload x
  garithmetize (G.K1 x) = arithmetize x
  gpayload (G.K1 x) = payload x
  grestore f = G.K1 (restore f)

type family Domain a where
  Domain (x -> y) = (x, Domain y)
  Domain x = Proxy (Context x)

type family Range a where
  Range (x -> y) = Range y
  Range y = y

class (LayoutFunctor (Layout (Range f)), Domain (Range f) ~ Proxy (Context (Range f))) => SymbolicFunction f where
  -- | Converts a function to a symbolic context.
  apply :: f -> Domain f -> (Context (Range f)) (Layout (Range f))

instance {-# INCOHERENT #-} SymbolicFunction y => SymbolicFunction (x -> y) where
  apply f (x, y) = apply (f x) y

instance {-# OVERLAPPABLE #-} (SymbolicData x, Range x ~ x, Domain x ~ Proxy (Context x)) => SymbolicFunction x where
  apply x _ = arithmetize x
