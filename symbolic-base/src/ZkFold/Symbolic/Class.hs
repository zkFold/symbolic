{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Class where

import Data.Binary (Binary)
import Data.Foldable (Foldable)
import Data.Functor.Rep (Rep, Representable)
import Data.Set (Set, singleton)
import Data.Traversable (Traversable)
import GHC.Generics (Par1 (..), (:*:) (..))
import Numeric.Natural (Natural)
import Data.Type.Equality (type (~))

import ZkFold.Algebra.Class
import ZkFold.Data.FromList (FromList)
import GHC.Integer (Integer)
import ZkFold.Data.Eq (BooleanOf)
import qualified Data.Eq as Haskell
import qualified Data.Ord as Haskell
import qualified Data.Enum as Haskell
import ZkFold.Data.Ord (Ord(OrderingOf))
import qualified Data.Bool as Haskell
import GHC.Stack (HasCallStack)
import Data.Function (id, ($), (.))
import Data.Functor ((<$>))

-- | @LookupTable f@ is a type of compact @f@-ary lookup table descriptions
-- using ideas from relational algebra.
data LookupTable f where
  -- | @Ranges@ describes a set of disjoint segments of the base field.
  Ranges :: Set (Natural, Natural) -> LookupTable Par1
  -- | @Product t u@ is a cartesian product of tables @t@ and @u@.
  Product :: LookupTable f -> LookupTable g -> LookupTable (f :*: g)
  -- | @Plot f x@ is a plot of a function @f@ with @x@ as a domain.
  Plot
    :: (Representable f, FromList f, Binary (Rep f), Foldable g)
    => (forall w. PrimeField w => f w -> g w)
    -> LookupTable f
    -> LookupTable (f :*: g)

type Poly a = forall b. Ring b => (a -> b) -> b

data Constraint a where
  Polynomial :: Poly a -> Constraint a
  Lookup :: Traversable f => LookupTable f -> f a -> Constraint a

infix 2 =!=

(=!=) :: Poly a -> Poly a -> Constraint a
p =!= q = Polynomial (p - q)

rangeTable :: Natural -> LookupTable Par1
rangeTable = Ranges . singleton . (0,)

(<!) :: a -> Natural -> Constraint a
x <! u = Lookup (rangeTable u) (Par1 x)

class PrimeField c => Symbolic c where
  constrain :: HasCallStack => Constraint c -> c -> c

assigned :: forall c. Symbolic c => Poly c -> c
assigned p = let x = p id in constrain (($ x) =!= p) x

plot
  :: (Symbolic c, Representable f, Binary (Rep f))
  => (FromList f, Traversable f, Traversable g)
  => (forall a. PrimeField a => f a -> g a)
  -> LookupTable f -> f c -> g c
plot f t x = let y = f x in constrain (Lookup (Plot f t) (x :*: y)) <$> y

-- | Symbolic field with decidable equality and ordering
-- is called an ``arithmetic'' field.
type Arithmetic a =
  ( Symbolic a
  , IntegralOf a ~ Integer
  , ToConstant a
  , Const a ~ Natural
  , BooleanOf a ~ Haskell.Bool
  , OrderingOf a ~ Haskell.Ordering
  , Haskell.Eq a
  , Haskell.Ord a
  , Haskell.Enum a
  )
