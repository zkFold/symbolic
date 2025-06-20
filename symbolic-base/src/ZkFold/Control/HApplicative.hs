{-# LANGUAGE TypeOperators #-}

module ZkFold.Control.HApplicative where

import Data.Function (const, ($), (.))
import GHC.Generics (U1 (..), (:*:) (..))
import ZkFold.Data.HFunctor

{- | A higher-order functor with application, providing operations to apply a
function of type @(forall a. f a -> g a -> ...)@ of arbitrary arity to
arguments of corresponding types @f a@, @g a@, ...
-}
class HFunctor c => HApplicative c where
  {-# MINIMAL (hpure | hunit), (hap | hliftA2 | hpair) #-}

  {- | Lift a proxy functor into the structure. If 'hunit' is specified instead,
  a default definition is available. The following is expected to hold:

  [Definition] @'hpure' x == 'hmap' ('const' x) 'hunit'@
  -}
  hpure :: (forall a. f a) -> c f
  hpure f = hmap (const f) hunit

  {- | Lift a concrete proxy functor @'U1'@ into the structure.
  Note that it is almost always better to define @'hpure'@ instead and rely
  on the default implementation of @'hunit'@.
  The following is expected to hold:

  [Definition] @'hunit' == 'hpure' 'U1'@
  -}
  hunit :: c U1
  hunit = hpure U1

  {- | Sequential application. It is hard to find the legitimate usecase for
  this function; this is only provided for comparison with classic
  @'Applicative'@ class.

  The default definition is via @'hpair'@. The following is expected to hold:

  [Definition] @'hap' t f == 'hmap' (\\('Transform' g ':*:' x) -> g x) ('hpair' t x)@
  -}
  hap :: c (Transform f g) -> c f -> c g
  hap t = hmap (\(Transform g :*: x) -> g x) . hpair t

  {- | Applies a binary function of type @(forall a. f a -> g a -> h a)@ to a
  pair of arguments of types @c f@ and @c g@,
  yielding the result of type @c h@.

  If @'hap'@ is specified instead, a default definition is available.
  The following is expected to hold:

  [Definition] @'hliftA2' f x y == 'hap' ('hmap' ('Transform' '.' f) x) y@
  [Compatibility] @'hmap' f x == 'hliftA2' ('const' f) 'hunit' x@
  -}
  hliftA2 :: (forall a. f a -> g a -> h a) -> c f -> c g -> c h
  hliftA2 f = hap . hmap (Transform . f)

  {- | Joins two structures, preserving the outputs. Note that it is almost
  always better to define @'hliftA2'@ instead and rely on the default
  implementation of @'hpair'@. The following is expected to hold:

  [Definition] @'hpair' x y == 'hliftA2' (':*:') x y@
  [Associativity] @'hliftA2' (\\(a ':*:' b) c -> a ':*:' (b ':*:' c)) ('hpair' x y) z == 'hpair' x ('hpair' y z)@
  [Left identity] @'hliftA2' 'const' x 'hunit' == x@
  [Right identity] @'hliftA2' ('const' 'id') 'hunit' y == y@
  -}
  hpair :: c f -> c g -> c (f :*: g)
  hpair = hliftA2 (:*:)

-- | A newtype wrapper for natural transformations used in @'hap'@ definition.
newtype Transform f g a = Transform {runTransform :: f a -> g a}

{- | If @'hap'@ and @'hpure'@ do not rely on @'hmap'@ (i.e. at least are not
implemented by default), this function can be used to implement @'hmap'@.
-}
hmapA :: HApplicative c => (forall a. f a -> g a) -> c f -> c g
hmapA g = hap $ hpure (Transform g)

{- | If @'hliftA2'@ and @'hunit'@ do not rely on @'hmap'@ (at least, @'hliftA2'@
is not implemented by default), this function can be used to implement @'hmap'@.
-}
hliftA1 :: HApplicative c => (forall a. f a -> g a) -> c f -> c g
hliftA1 g = hliftA2 (const g) hunit

{- | Applies a ternary natural transformation
to a triple of arguments behind an @'HApplicative'@.
-}
hliftA3 :: HApplicative c => (forall a. f a -> g a -> h a -> i a) -> c f -> c g -> c h -> c i
hliftA3 i f g = hliftA2 (\(x :*: y) -> i x y) (hpair f g)
