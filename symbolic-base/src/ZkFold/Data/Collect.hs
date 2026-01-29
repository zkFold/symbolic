{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Data.Collect where

import Data.Foldable (Foldable, foldMap)
import Data.Function (const, (.))
import Data.Kind (Type)
import Data.Monoid (Monoid, mempty)
import Data.Semigroup ((<>))
import GHC.Generics

import ZkFold.Data.Vector (Vector)

class Monoid m => Collect m a where
  collect :: a -> m
  default collect :: (Generic a, GCollect m (Rep a)) => a -> m
  collect = gcollect . from

instance (Collect m (f a), Collect m (g a)) => Collect m ((f :*: g) a)

newtype Collection f a = Collection {collection :: f a}

instance (Collect m a, Foldable f) => Collect m (Collection f a) where
  collect = foldMap collect . collection

deriving via
  (Collection (Vector n) a)
  instance
    Collect m a => Collect m (Vector n a)

deriving via
  (Collection f ((g :: Type -> Type) a))
  instance
    (Foldable f, Collect m (g a)) => Collect m ((f :.: g) a)

class Monoid m => GCollect m f where
  gcollect :: f a -> m

instance Monoid m => GCollect m U1 where
  gcollect = const mempty

instance Collect m a => GCollect m (K1 i a) where
  gcollect (K1 a) = collect a

instance GCollect m f => GCollect m (M1 i c f) where
  gcollect (M1 f) = gcollect f

instance (GCollect m f, GCollect m g) => GCollect m (f :+: g) where
  gcollect (L1 f) = gcollect f
  gcollect (R1 g) = gcollect g

instance (GCollect m f, GCollect m g) => GCollect m (f :*: g) where
  gcollect (f :*: g) = gcollect f <> gcollect g
