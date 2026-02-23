{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Polynomial (lookup-free) encoding of bounded integers.
--
-- 'PolyBounded n ctx' wraps a 'FieldElement' and implements comparison via
-- polynomial bit-decomposition (@b*(b-1)=0@ per bit) instead of lookup range
-- constraints.  Every constraint produced is a polynomial constraint, so values
-- of this type are safe to use inside Protostar IVC step circuits where lookup
-- constraints are not folded by the accumulator.
--
-- Arithmetic (+, -, negate) is plain field arithmetic — no carry expansion, no
-- lookups.  Range safety for comparison is enforced by the polynomial expansion
-- witness inside '(>=)'.
module ZkFold.Symbolic.Ledger.Types.Polynomial (
  PolyBounded (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic, Generic1, Par1 (..))
import GHC.TypeNats (KnownNat, Natural)
import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (value)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.HFunctor.Classes (HEq, HShow)
import ZkFold.Data.Ord
import ZkFold.Symbolic.Class (Symbolic (..), fromCircuit2F)
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (..), bool)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Combinators (polynomialExpansion)
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Ord (Ordering)
import ZkFold.Symbolic.MonadCircuit (at, newAssigned, newConstrained)
import Prelude hiding (
  Bool,
  Eq,
  Ord,
  Ordering,
  negate,
  (&&),
  (*),
  (+),
  (-),
  (/=),
  (<),
  (<=),
  (==),
  (>),
  (>=),
  (||),
 )
import qualified Prelude as Haskell

import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Types.Orphans ()

-- | An @n@-bit bounded integer stored as a 'FieldElement'.
--
-- All operations except comparison use plain field arithmetic (polynomial
-- degree ≤ 2).  The comparison '(>=)' is implemented by witnessing a boolean
-- flag and polynomially range-checking the appropriate difference using
-- 'polynomialExpansion'.  No lookup constraints are generated.
newtype PolyBounded (n :: Natural) ctx = PolyBounded
  { unPolyBounded :: FieldElement ctx
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

deriving stock instance HEq ctx => Haskell.Eq (PolyBounded n ctx)

deriving stock instance HShow ctx => Haskell.Show (PolyBounded n ctx)

deriving newtype instance ToJSON (PolyBounded n RollupBFInterpreter)

deriving newtype instance FromJSON (PolyBounded n RollupBFInterpreter)

deriving newtype instance KnownNat n => ToSchema (PolyBounded n RollupBFInterpreter)

-- ---------------------------------------------------------------------------
-- Arithmetic instances — all field arithmetic, no range checks, no lookups
-- ---------------------------------------------------------------------------

instance Symbolic ctx => Zero (PolyBounded n ctx) where
  zero = PolyBounded zero

instance Symbolic ctx => AdditiveSemigroup (PolyBounded n ctx) where
  PolyBounded x + PolyBounded y = PolyBounded (x + y)

instance Symbolic ctx => AdditiveMonoid (PolyBounded n ctx)

instance Symbolic ctx => AdditiveGroup (PolyBounded n ctx) where
  negate (PolyBounded x) = PolyBounded (negate x)
  PolyBounded x - PolyBounded y = PolyBounded (x - y)

-- | Mirrors 'FieldElement' — use {-# INCOHERENT #-} to avoid overlap with
-- 'FromConstant a a' when @k ~ PolyBounded n ctx@.
instance {-# INCOHERENT #-} (Symbolic ctx, FromConstant k (BaseField ctx)) => FromConstant k (PolyBounded n ctx) where
  fromConstant = PolyBounded . fromConstant

-- | Self-case that takes priority over the INCOHERENT instance above.
instance {-# OVERLAPPING #-} FromConstant (PolyBounded n ctx) (PolyBounded n ctx)

-- | General scaling via the base field — mirrors 'FieldElement'.
instance (Symbolic ctx, Scale k (BaseField ctx)) => Scale k (PolyBounded n ctx) where
  scale k (PolyBounded x) = PolyBounded (scale k x)

-- | Self-scaling — explicit body to avoid needing 'MultiplicativeSemigroup'.
instance {-# OVERLAPPING #-} Symbolic ctx => Scale (PolyBounded n ctx) (PolyBounded n ctx) where
  scale (PolyBounded k) (PolyBounded x) = PolyBounded (scale k x)

-- ---------------------------------------------------------------------------
-- Equality — delegates to FieldElement equality (polynomial, uses isZero gadget)
-- ---------------------------------------------------------------------------

instance Symbolic ctx => Eq (PolyBounded n ctx) where
  type BooleanOf (PolyBounded n ctx) = Bool ctx
  PolyBounded a == PolyBounded b = a == b
  PolyBounded a /= PolyBounded b = a /= b

-- ---------------------------------------------------------------------------
-- Comparison — polynomial GEQ, no lookups
-- ---------------------------------------------------------------------------

-- | Polynomial greater-than-or-equal comparison for 'PolyBounded' values.
--
-- Witnesses a boolean @ge ∈ {0,1}@ and adds polynomial constraints:
--
-- * @ge = 1@: proves @a - b ∈ [0, 2^n)@ via @n@ boolean constraints.
-- * @ge = 0@: proves @b - a - 1 ∈ [0, 2^n)@ via @n@ boolean constraints.
--
-- This is sound because both @a - b@ and @b - a - 1@ cannot simultaneously
-- fit in @[0, 2^n)@ when the field order @p >> 2^n@ (which holds for
-- 'RollupBF').
polyGEQ
  :: forall n ctx
   . (Symbolic ctx, KnownNat n)
  => FieldElement ctx
  -> FieldElement ctx
  -> Bool ctx
polyGEQ (FieldElement fa) (FieldElement fb) =
  Bool $
    fromCircuit2F fa fb $
      \(Par1 va) (Par1 vb) -> do
        -- Witness: 1 if a >= b (comparing canonical integer representations).
        -- Uses ZkFold's 'Ord' on 'IntegralOf (WitnessField ctx)' which holds
        -- via 'Euclidean' (from 'PrimeField').  'bool zero one' uses
        -- 'Conditional (BooleanOf (WitnessField ctx)) (WitnessField ctx)'
        -- which holds via 'Field' (superclass of 'PrimeField').
        ge <-
          newConstrained
            (\x v -> x v * (x v - one)) -- boolean: ge*(ge-1) = 0
            (bool zero one (toIntegral (at va) >= toIntegral (at vb)))
        -- d = ge*(a-b) + (1-ge)*(b-a-1)
        -- d ∈ [0,2^n) is provable iff the comparison witness is honest.
        --
        -- Introduce diff = a - b so that d = ge*(2*diff+1) - diff - 1,
        -- which uses only one degree-2 monomial (ge*diff) and is a valid PLONK gate.
        diff <- newAssigned (\x -> x va - x vb)
        d <-
          newAssigned
            ( \x ->
                x ge * (x diff + x diff + one) - x diff - one
            )
        -- n polynomial boolean constraints + 1 reconstruction constraint
        _ <- polynomialExpansion (value @n) d
        return (Par1 ge)

instance (Symbolic ctx, KnownNat n) => Ord (PolyBounded n ctx) where
  type OrderingOf (PolyBounded n ctx) = Ordering ctx

  ordering ltCase eqCase gtCase o =
    bool (bool ltCase eqCase (o == eq)) gtCase (o == gt)

  compare x y = bool (bool lt eq (x == y)) gt (x > y)

  x >= y = polyGEQ @n (unPolyBounded x) (unPolyBounded y)
  x > y = polyGEQ @n (unPolyBounded x) (unPolyBounded y) && x /= y
  x <= y = polyGEQ @n (unPolyBounded y) (unPolyBounded x)
  x < y = polyGEQ @n (unPolyBounded y) (unPolyBounded x) && x /= y
