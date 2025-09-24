{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.V2 where

import Data.Set (Set)
import GHC.Generics (Par1, (:*:))
import Data.Functor.Rep (Representable, Rep)
import ZkFold.Data.FromList (FromList)
import Data.Binary (Binary)
import Data.Foldable (Foldable)
import ZkFold.Algebra.Class (PrimeField, Algebra)
import Numeric.Natural (Natural)
import Control.DeepSeq (NFData)

-- | @LookupTable a f@ is a type of compact lookup table descriptions using ideas from relational algebra.
-- @a@ is a base field type, @f@ is a functor such that @f a@ is a type whose subset this lookup table describes.
data LookupTable f where
  -- | @Ranges@ describes a set of disjoint segments of the base field.
  Ranges :: Set (Natural, Natural) -> LookupTable Par1
  -- | @Product t u@ is a cartesian product of tables @t@ and @u@.
  Product :: LookupTable f -> LookupTable g -> LookupTable (f :*: g)
  -- | @Plot f x@ is a plot of a function @f@ with @x@ as a domain.
  Plot
    :: (Representable f, FromList f, Binary (Rep f), Foldable g)
    => (forall w. PrimeField w => f w -> g w)
    -> LookupTable f -> LookupTable (f :*: g)

data Constraint a where
  Polynomial :: (forall b. Algebra a b => b) -> Constraint a
  Lookup :: LookupTable f -> f a -> Constraint a

-- TODO: Get rid of NFData constraint
class (NFData c, PrimeField c) => Symbolic c where
  constrain :: Constraint c -> c -> c
