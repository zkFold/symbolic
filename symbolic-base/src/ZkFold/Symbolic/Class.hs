{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module ZkFold.Symbolic.Class (
  LookupTable (..),
  Poly,
  Constraint (..),
  (=!=),
  rangeTable,
  (!<=),
  Symbolic (..),
  Arithmetic,
  assigned,
  plot,
) where

import Control.Applicative (pure)
import Data.Binary (Binary)
import Data.Foldable (Foldable, toList)
import Data.Function (id, ($), (.))
import Data.Functor ((<$>))
import Data.Functor.Rep (Rep, Representable)
import Data.List (intercalate, (++))
import Data.Semialign (Semialign, align)
import Data.Set (Set, singleton)
import Data.These (mergeThese)
import Data.Traversable (Traversable)
import Data.Type.Ord (type (<=))
import GHC.Generics (Par1 (..), (:*:) (..))
import GHC.Integer (Integer)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import Text.Show (Show (..))

import ZkFold.Algebra.Class
import ZkFold.Algebra.Polynomial.Multivariate.Expression
import ZkFold.Data.Bool (all, any, (&&))
import ZkFold.Data.Eq (BooleanOf, (==))
import ZkFold.Data.FromList (FromList)
import ZkFold.Data.Ord ((<=))
import ZkFold.Prelude (assert)

-- | @LookupTable f@ is a type of compact @f@-ary lookup table descriptions
-- using ideas from relational algebra.
data LookupTable f where
  -- | @Ranges@ describes a set of disjoint segments of the base field.
  Ranges :: Set (Natural, Natural) -> LookupTable Par1
  -- | @Product t u@ is a cartesian product of tables @t@ and @u@.
  Product :: LookupTable f -> LookupTable g -> LookupTable (f :*: g)
  -- | @Plot f x@ is a plot of a function @f@ with @x@ as a domain.
  Plot
    :: (Representable f, FromList f, Binary (Rep f), Semialign g, Foldable g)
    => (forall w. (PrimeField w, 3 <= Order w) => f w -> g w)
    -> LookupTable f
    -> LookupTable (f :*: g)

lookupTable
  :: (PrimeField a, 3 <= Order a) => LookupTable f -> f a -> BooleanOf a
lookupTable (Ranges rs) (Par1 (toIntegral -> x)) =
  any (\(lo, hi) -> fromConstant lo <= x && x <= fromConstant hi) rs
lookupTable (Product k l) (f :*: g) = lookupTable k f && lookupTable l g
lookupTable (Plot f l) (xs :*: ys) =
  lookupTable l xs && all ((== zero) . mergeThese (-)) (f xs `align` ys)

instance Show (LookupTable f) where
  show (Ranges rs) =
    intercalate
      " ∪ "
      ["[" ++ show lo ++ "; " ++ show hi ++ "]" | (lo, hi) <- toList rs]
  show (Product k l) = "(" ++ show k ++ ") x (" ++ show l ++ ")"
  show (Plot _ l) = "{ (x, f(x)) | x ∈ " ++ show l ++ "}"

type Poly a = forall b. Ring b => (a -> b) -> b

data Constraint a where
  Polynomial :: Poly a -> Constraint a
  Lookup :: Traversable f => LookupTable f -> f a -> Constraint a

evalConstraint :: (PrimeField a, 3 <= Order a) => Constraint a -> BooleanOf a
evalConstraint (Polynomial p) = p id == zero
evalConstraint (Lookup t f) = lookupTable t f

instance Show a => Show (Constraint a) where
  show (Polynomial p) = show (p pure :: Polynomial Integer a) ++ " =!= 0"
  show (Lookup t f) = show (toList f) ++ " ∈! " ++ show t

infix 2 =!=

(=!=) :: Poly a -> Poly a -> Constraint a
p =!= q = Polynomial (p - q)

rangeTable :: Natural -> LookupTable Par1
rangeTable = Ranges . singleton . (0,)

(!<=) :: a -> Natural -> Constraint a
x !<= u = Lookup (rangeTable u) (Par1 x)

newtype Failed a = Failed (Constraint a)

instance Show a => Show (Failed a) where
  show (Failed c) = "Failed constraint: " ++ show c

class (PrimeField c, 3 <= Order c) => Symbolic c where
  constrain :: HasCallStack => Constraint c -> c -> c
  default constrain
    :: (Decidable c, Show c, HasCallStack) => Constraint c -> c -> c
  constrain c = assert (evalConstraint c) (Failed c)

-- | 'Decidable' 'Symbolic' field is called 'Arithmetic'.
type Arithmetic a = (Symbolic a, Decidable a)

assigned :: (Symbolic c, HasCallStack) => Poly c -> c
assigned p = let x = p id in constrain (($ x) =!= p) x

plot
  :: (HasCallStack, Symbolic c, Representable f, Binary (Rep f))
  => (FromList f, Traversable f, Semialign g, Traversable g)
  => (forall a. (PrimeField a, 3 <= Order a) => f a -> g a)
  -> LookupTable f
  -> f c
  -> g c
plot f t x = let y = f x in constrain (Lookup (Plot f t) (x :*: y)) <$> y
