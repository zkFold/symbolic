{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.MonadCircuit where

import           Control.Monad                                     (Monad (return))
import           Data.Binary                                       (Binary)
import           Data.Function                                     ((.))
import           Data.Functor.Rep                                  (Rep, Representable)
import           Data.Kind                                         (Type)
import           Prelude                                           (Foldable, Integer)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field                   (Zp)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Lookup

-- | A 'ResidueField' is a 'FiniteField'
-- backed by a 'Euclidean' integral type.
class (FiniteField a, Euclidean (IntegralOf a)) => ResidueField a where
  type IntegralOf a :: Type
  fromIntegral :: IntegralOf a -> a
  toIntegral :: a -> IntegralOf a

instance PrimeField (Zp p) => ResidueField (Zp p) where
  type IntegralOf (Zp p) = Integer
  fromIntegral = fromConstant
  toIntegral = fromConstant . toConstant

-- | A type of witness builders. @i@ is a type of variables.
--
-- Witness builders should support all the operations of witnesses,
-- and in addition there should be a corresponding builder for each variable.
class ResidueField w => Witness i w | w -> i where
  -- | @at x@ is a witness builder whose value is equal to the value of @x@.
  at :: i -> w

-- | A type of polynomial expressions.
-- @var@ is a type of variables, @a@ is a base field.
--
-- A function is a polynomial expression if, given an arbitrary algebra @x@ over
-- @a@ and a function mapping known variables to their witnesses, it computes a
-- new value in that algebra.
--
-- NOTE: the property above is correct by construction for each function of a
-- suitable type, you don't have to check it yourself.
type ClosedPoly var a = forall x . Algebra a x => (var -> x) -> x

-- | A type of constraints for new variables.
-- @var@ is a type of variables, @a@ is a base field.
--
-- A function is a constraint for a new variable if, given an arbitrary algebra
-- @x@ over @a@, a function mapping known variables to their witnesses in that
-- algebra and a new variable, it computes the value of a constraint polynomial
-- in that algebra.
--
-- NOTE: the property above is correct by construction for each function of a
-- suitable type, you don't have to check it yourself.
type NewConstraint var a = forall x . Algebra a x => (var -> x) -> var -> x

-- | A monadic DSL for constructing arithmetic circuits.
-- @var@ is a type of variables, @a@ is a base field, @w@ is a type of witnesses
-- and @m@ is a monad for constructing the circuit.
--
-- DSL provides the following guarantees:
--
-- * Constraints never reference undefined variables;
-- * Variables with equal witnesses are reused as much as possible;
-- * Variables with different witnesses are different;
-- * There is an order in which witnesses can be generated.
--
-- However, DSL does NOT provide the following guarantees (yet):
--
-- * That provided witnesses satisfy the provided constraints. To check this,
--   you can use 'ZkFold.Symbolic.Compiler.ArithmeticCircuit.checkCircuit'.
-- * That introduced constraints are supported by the zk-SNARK utilized for later proving.
class ( Monad m, FromConstant a var
      , FromConstant a w, Scale a w, Witness var w
      ) => MonadCircuit var a w m | m -> var, m -> a, m -> w where
  -- | Creates new variable from witness.
  --
  -- NOTE: this does not add any constraints to the system,
  -- use 'rangeConstraint' or 'constraint' to add them.
  unconstrained :: w -> m var

  -- | Adds new polynomial constraint to the system.
  -- E.g., @'constraint' (\\x -> x i)@ forces variable @var@ to be zero.
  --
  -- NOTE: it is not checked (yet) whether provided constraint is in
  -- appropriate form for zkSNARK in use.
  constraint :: ClosedPoly var a -> m ()

  -- | Adds new range constraint to the system.
  -- E.g., @'rangeConstraint' var B@ forces variable @var@ to be in range \([0; B]\).
  rangeConstraint :: var -> a -> m ()

  -- | Adds new lookup function to the system.
  -- For example, @'registerFunction' f @ stores the function @f@.
  registerFunction :: (Representable f, Binary (Rep f), Foldable g)
    => (forall x. ResidueField x => f x -> g x) -> m (FunctionId (f a -> g a))

  -- | Creates new variable given a polynomial witness
  -- AND adds a corresponding polynomial constraint.
  --
  -- E.g., @'newAssigned' (\\x -> x i + x j)@ creates new variable @k@
  -- whose value is equal to \(x_i + x_j\)
  -- and a constraint \(x_i + x_j - x_k = 0\).
  --
  -- NOTE: this adds a polynomial constraint to the system.
  --
  -- NOTE: is is not checked (yet) whether the corresponding constraint is in
  -- appropriate form for zkSNARK in use.
  newAssigned :: ClosedPoly var a -> m var
  newAssigned p = newConstrained (\x var -> p x - x var) (p at)

-- | Creates new variable from witness constrained with an inclusive upper bound.
-- E.g., @'newRanged' b (\\x -> x var - one)@ creates new variable whose value
-- is equal to @x var - one@ and which is expected to be in range @[0..b]@.
--
-- NOTE: this adds a range constraint to the system.
newRanged :: MonadCircuit var a w m => a -> w -> m var
newRanged upperBound witness = do
  v <- unconstrained witness
  rangeConstraint v upperBound
  return v

-- | Creates new variable from witness constrained by a polynomial.
-- E.g., @'newConstrained' (\\x i -> x i * (x i - one)) (\\x -> x j - one)@
-- creates new variable whose value is equal to @x j - one@ and which is
-- expected to be a root of the polynomial @x i * (x i - one)@.
--
-- NOTE: this adds a polynomial constraint to the system.
--
-- NOTE: it is not checked (yet) whether provided constraint is in
-- appropriate form for zkSNARK in use.
newConstrained :: MonadCircuit var a w m => NewConstraint var a -> w -> m var
newConstrained poly witness = do
  v <- unconstrained witness
  constraint (`poly` v)
  return v
