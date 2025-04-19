module ZkFold.Data.HFunctor where

-- | A type @c@ is a higher-order functor (@'HFunctor'@) if it provides a
-- function @'hmap'@ which, given any two types @f@ and @g@ of kind @k -> Type@
-- lets you apply any function from @(forall a. f a -> g a)@ to turn an @c f@
-- into an @c g@, preserving the structure of @c@.
class HFunctor c where
  -- | Applies a function of type @(forall a. f a -> g a)@
  -- to a value of type @c f@, where @c@ is a higher-order functor,
  -- to produce a value of type @c g@. Usual laws of functors should hold:
  --
  -- [Identity] @'hmap' 'id' x == x@
  -- [Composition] @'hmap' (f '.' g) x == 'hmap' f ('hmap' g x)@
  hmap :: (forall a. f a -> g a) -> c f -> c g
