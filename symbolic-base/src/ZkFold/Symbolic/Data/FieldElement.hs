{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.FieldElement where

import Control.DeepSeq (NFData)
import Data.Foldable (foldr)
import Data.Function (($), (.))
import Data.Functor (Functor, fmap, (<$>))
import Data.Tuple (snd)
import GHC.Generics (Generic, Par1 (..))
import Test.QuickCheck (Arbitrary (..))
import Prelude (Integer)
import qualified Prelude as Haskell

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.HFunctor.Classes (HEq, HNFData, HShow)
import ZkFold.Data.Package (Package, unpacked)
import ZkFold.Data.Vector (Vector, fromVector, unsafeToVector)
import ZkFold.Symbolic.Class (Symbolic (..), embed, fromCircuit2F, fromCircuit3F, fromCircuitF, symbolicF)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators (expansion, horner, runInvert, runInvertOrFail)
import ZkFold.Symbolic.Data.Input
import ZkFold.Symbolic.Data.Ord
import ZkFold.Symbolic.Data.Vec (Vec (..))
import ZkFold.Symbolic.Interpreter (Interpreter (..))
import ZkFold.Symbolic.MonadCircuit (Witness (..), newAssigned, newConstrained)

newtype FieldElement c = FieldElement {fromFieldElement :: c Par1}
  deriving Generic
  deriving (SymbolicData, SymbolicInput) via (Vec Par1)
  deriving (Eq, Ord) via (Vec Par1 c)

fieldElements :: (Package c, Functor f) => c f -> f (FieldElement c)
fieldElements = fmap FieldElement . unpacked

deriving stock instance HShow c => Haskell.Show (FieldElement c)

deriving stock instance HEq c => Haskell.Eq (FieldElement c)

deriving stock instance (HEq c, Haskell.Ord (c Par1)) => Haskell.Ord (FieldElement c)

deriving newtype instance HNFData c => NFData (FieldElement c)

instance {-# INCOHERENT #-} (Symbolic c, FromConstant k (BaseField c)) => FromConstant k (FieldElement c) where
  fromConstant = FieldElement . embed . Par1 . fromConstant

instance ToConstant (FieldElement (Interpreter a)) where
  type Const (FieldElement (Interpreter a)) = a
  toConstant (FieldElement (Interpreter (Par1 x))) = x

instance Symbolic c => Exponent (FieldElement c) Natural where
  (^) = natPow

instance Symbolic c => Exponent (FieldElement c) Integer where
  (^) = intPowF

instance (Symbolic c, Scale k (BaseField c)) => Scale k (FieldElement c) where
  scale k (FieldElement c) = FieldElement $ fromCircuitF c $ \(Par1 i) ->
    Par1 <$> newAssigned (\x -> fromConstant (scale k one :: BaseField c) * x i)

instance {-# OVERLAPPING #-} FromConstant (FieldElement c) (FieldElement c)

instance {-# OVERLAPPING #-} Symbolic c => Scale (FieldElement c) (FieldElement c)

instance Symbolic c => MultiplicativeSemigroup (FieldElement c) where
  FieldElement x * FieldElement y = FieldElement $
    fromCircuit2F x y $
      \(Par1 i) (Par1 j) -> Par1 <$> newAssigned (\w -> w i * w j)

instance Symbolic c => MultiplicativeMonoid (FieldElement c) where
  one = FieldElement $ embed (Par1 one)

instance Symbolic c => AdditiveSemigroup (FieldElement c) where
  FieldElement x + FieldElement y = FieldElement $
    fromCircuit2F x y $
      \(Par1 i) (Par1 j) -> Par1 <$> newAssigned (\w -> w i + w j)

instance Symbolic c => Zero (FieldElement c) where
  zero = FieldElement $ embed (Par1 zero)

instance Symbolic c => AdditiveMonoid (FieldElement c)

instance Symbolic c => AdditiveGroup (FieldElement c) where
  negate (FieldElement x) = FieldElement $ fromCircuitF x $ \(Par1 i) ->
    Par1 <$> newAssigned (\w -> negate (w i))

  FieldElement x - FieldElement y = FieldElement $
    fromCircuit2F x y $
      \(Par1 i) (Par1 j) -> Par1 <$> newAssigned (\w -> w i - w j)

instance Symbolic c => Semiring (FieldElement c)

instance Symbolic c => Ring (FieldElement c)

instance Symbolic c => Field (FieldElement c) where
  finv (FieldElement x) =
    FieldElement $
      symbolicF x (\(Par1 v) -> Par1 (finv v)) $
        fmap snd . runInvert

-- | Optimized field inversion that uses only 1 constraint.
-- IMPORTANT: This assumes the input is non-zero. If the input is zero,
-- the circuit will be unsatisfiable.
-- Use this only when you can guarantee the input is non-zero.
finvOrFail :: Symbolic c => FieldElement c -> FieldElement c
finvOrFail (FieldElement x) =
  FieldElement $
    symbolicF x (\(Par1 v) -> Par1 (finv v)) runInvertOrFail

-- | Compute (a + b) / den in a single constraint (plus the combined constraint).
-- Instead of computing:
--   sum = a + b        (1 constraint)
--   result = sum / den (1 constraint for inv + 1 for mul = 2 constraints)
-- We use the combined constraint:
--   result * den = a + b  (single constraint)
-- 
-- Total: 1 constraint instead of 3.
-- IMPORTANT: This assumes den is non-zero.
sumDivOrFail :: Symbolic c => FieldElement c -> FieldElement c -> FieldElement c -> FieldElement c
sumDivOrFail (FieldElement a) (FieldElement b) (FieldElement d) =
  FieldElement $
    fromCircuit3F a b d $ \(Par1 ai) (Par1 bi) (Par1 di) ->
      -- Witness: result = (a + b) / d
      -- Constraint: result * d - a - b = 0
      Par1 Haskell.<$> newConstrained
        (\x result -> x result * x di - x ai - x bi)
        ((at ai + at bi) // at di)

instance
  ( KnownNat (Order (FieldElement c))
  , KnownNat (NumberOfBits (FieldElement c))
  )
  => Finite (FieldElement c)
  where
  type Order (FieldElement c) = Order (BaseField c)

instance Symbolic c => BinaryExpansion (FieldElement c) where
  type Bits (FieldElement c) = c (Vector (NumberOfBits (BaseField c)))
  binaryExpansion (FieldElement c) =
    hmap unsafeToVector $
      symbolicF
        c
        (padBits n . fmap fromConstant . binaryExpansion . toConstant . unPar1)
        (expansion n . unPar1)
   where
    n = numberOfBits @(BaseField c)
  fromBinary bits =
    FieldElement $
      symbolicF bits (Par1 . foldr (\x y -> x + y + y) zero) $
        fmap Par1 . horner . fromVector

instance (Symbolic c, Arbitrary (BaseField c)) => Arbitrary (FieldElement c) where
  arbitrary = FieldElement . embed . Par1 <$> arbitrary
