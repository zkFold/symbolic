{-# LANGUAGE TypeOperators #-}

module ZkFold.Data.Package where

import           Data.Foldable        (Foldable)
import           Data.Function        ((.))
import           Data.Functor         (Functor (..))
import           GHC.Generics         (Par1 (..), (:.:) (..))

import           ZkFold.Data.HFunctor (HFunctor (..))

-- | A @'Package'@ is a higher-order functor (@'HFunctor'@) which allows to
-- (un)pack layered structures of nested functors.
class HFunctor c => Package c where
  {-# MINIMAL (unpack | unpackWith), (pack | packWith) #-}

  -- | Unpacks the outer layer of a package.
  -- Note that it is almost always better to define 'unpackWith' instead
  -- and rely on the default implementation of 'unpack'.
  -- The following should hold:
  --
  -- [Definition] @'unpack' p == 'unpackWith' 'unComp1' p@
  unpack :: Functor f => c (f :.: g) -> f (c g)
  unpack = unpackWith unComp1

  -- | Given a way to peel the outer layer, unpacks it.
  -- If @'unpack'@ is specified instead, a default definition is available.
  -- The following should hold:
  --
  -- [Definition] @'unpackWith' f p == 'unpack' ('hmap' ('Comp1' '.' f) p)@
  -- [Compatibility] @'hmap' f p == 'unPar1' ('unpackWith' ('Par1' '.' f) p)@
  unpackWith :: Functor f => (forall a. h a -> f (g a)) -> c h -> f (c g)
  unpackWith f = unpack . hmap (Comp1 . f)

  -- | Packs the outer layer into the package.
  -- Note that it is almost always better to define 'packWith' instead
  -- and rely on the default implementation of 'pack'.
  -- The following should hold:
  --
  -- [Definition] @'pack' p == 'packWith' 'Comp1' p@
  -- [Inverse] @'pack' ('unpack' p) == p@
  pack :: (Foldable f, Functor f) => f (c g) -> c (f :.: g)
  pack = packWith Comp1

  -- | Given a way to merge the outer layer, packs it.
  -- If 'pack' is specified instead, a default definition if available.
  -- The following should hold:
  --
  -- [Definition] @'packWith' f p == 'hmap' (f '.' 'unComp1') ('pack' p)@
  -- [Compatibility] @'hmap' f p == 'packWith' (f . 'unPar1') ('Par1' p)@
  packWith :: (Foldable f, Functor f) => (forall a. f (g a) -> h a) -> f (c g) -> c h
  packWith f = hmap (f . unComp1) . pack

-- | Performs the full unpacking.
unpacked :: (Package c, Functor f) => c f -> f (c Par1)
unpacked = unpackWith (fmap Par1)

-- | Performs the full package.
packed :: (Package c, Foldable f, Functor f) => f (c Par1) -> c f
packed = packWith (fmap unPar1)
