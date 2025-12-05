{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

{- HLINT ignore "Use concatMap" -}
{- HLINT ignore "Use zipWithM" -}

module ZkFold.Symbolic.Data.UIntData where

import Data.Type.Equality (type (~))
import ZkFold.Symbolic.Data.Register
import ZkFold.Algebra.Number
import ZkFold.Data.Vector (Vector)
import GHC.Generics (Generic, Generic1, type (:*:) (..))
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Data.Collect (Collect)
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import ZkFold.Data.Eq (Eq, (==))
import ZkFold.Symbolic.Data.Input (SymbolicInput, isValid)
import ZkFold.Symbolic.Class (Symbolic, Arithmetic)
import ZkFold.Data.Ord (Ord, (<))
import ZkFold.Algebra.Class
import GHC.Integer (Integer)
import ZkFold.Algebra.Field (Zp, fromZp)
import ZkFold.Control.BackwardsState
import ZkFold.Symbolic.Data.Bool
import qualified Data.Vector as UV
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import Data.Functor ((<$>))
import Control.Applicative ((<*>), pure)
import Data.Traversable (sequence, traverse)
import Data.Function (($), (.))
import Data.Constraint (withDict, (:-), (\\))
import qualified ZkFold.Data.Vector as V
import Data.Semialign (zipWith)
import ZkFold.Control.Conditional (ifThenElse)
import GHC.Err (error)
import ZkFold.Data.Product (toPair)
import Data.Bool (otherwise)
import GHC.Real (even, odd)
import ZkFold.Symbolic.Data.ByteString
import Data.Constraint.Nat (plusCommutes, euclideanNat, timesCommutes)

data UIntData n k c = MkUIntData
  { highRegister :: Register (Mod n k) c
  , lowRegisters :: Vector (Div n k) (Register k c)
  }
  deriving (Generic, Generic1, SymbolicData, Collect (ConstrainedDatum c), Eq)

type KnownUIntData n k =
  (KnownNat n, KnownNat k, KnownNat (Div n k), KnownNat (Mod n k), 1 <= k)

instance KnownUIntData n k => SymbolicInput (UIntData n k)

instance (KnownUIntData n k, Symbolic c) => Ord (UIntData n k c)

instance {-# OVERLAPPING #-} FromConstant (UIntData n k c) (UIntData n k c)

instance {-# OVERLAPPING #-}
  Symbolic c => Scale (UIntData n k c) (UIntData n k c)

fromSemiEuclidean
  :: (SemiEuclidean a, FromConstant a c, KnownUIntData n k)
  => a -> UIntData n k c
fromSemiEuclidean x = evalBState x do
  MkUIntData
    <$> MkBState limb
    <*> sequence (pure $ MkBState limb)

instance
  (KnownUIntData n k, FromConstant Natural c)
  => FromConstant Natural (UIntData n k c) where
  fromConstant =
    withDict (pow2Nat @n) (fromSemiEuclidean . fromZp @(2 ^ n) . fromConstant)

instance
  (KnownUIntData n k, FromConstant Natural c)
  => FromConstant Integer (UIntData n k c) where
  fromConstant =
    withDict (pow2Nat @n) (fromSemiEuclidean . fromZp @(2 ^ n) . fromConstant)

uintDataToIntegral :: (KnownNat k, Symbolic c) => UIntData n k c -> IntegralOf c
uintDataToIntegral (MkUIntData hi lo) = regsToIntegral (V.reverse lo) hi

instance
  (KnownNat n, KnownNat k, Arithmetic a) => ToConstant (UIntData n k a) where
  type Const (UIntData n k a) = Zp (2 ^ n)
  toConstant = withDict (pow2Nat @n) (fromConstant . uintDataToIntegral)

instance (KnownUIntData n k, Symbolic c) => Scale Natural (UIntData n k c)

instance (KnownUIntData n k, Symbolic c) => Scale Integer (UIntData n k c)

singleAdd
  :: (KnownNat r, Symbolic c)
  => Register r c -> Register r c -> BState (Bool c) (Register r c)
singleAdd x y = MkBState \c -> regToBool <$> splitR (adder x y c)

instance
  (KnownUIntData n k, Symbolic c) => AdditiveSemigroup (UIntData n k c) where
  MkUIntData hi lo + MkUIntData hi' lo' = evalBState false do
    MkUIntData
      <$> singleAdd hi hi'
      <*> sequence (zipWith singleAdd lo lo')

instance (KnownNat (Div n k), Symbolic c) => Zero (UIntData n k c) where
  zero = MkUIntData zero zero

instance (KnownUIntData n k, Symbolic c) => AdditiveMonoid (UIntData n k c)

instance (KnownUIntData n k, Symbolic c) => AdditiveGroup (UIntData n k c) where
  negate (MkUIntData hi lo) = evalBState true do
    MkUIntData
      <$> singleNeg hi
      <*> traverse singleNeg lo
   where
    singleNeg :: KnownNat r => Register r c -> BState (Bool c) (Register r c)
    singleNeg x = MkBState \c ->
      let (y, c') = negateR x in (ifThenElse c y (not x), c && c')

instance
  (KnownUIntData n k, Symbolic c) => Exponent (UIntData n k c) Natural where
  (^) = natPow

instance Symbolic c => MultiplicativeSemigroup (UIntData n k c) where
  x * y = fromVector $ zipWith (*) (toVector x) (toVector y)
   where
    toVector :: UIntData n k c -> UV.Vector (FieldElement c)
    toVector = error "TODO"

    fromVector :: UV.Vector (FieldElement c) -> UIntData n k c
    fromVector = error "TODO"

instance
  (KnownUIntData n k, Symbolic c) => MultiplicativeMonoid (UIntData n k c) where
  one = fromConstant (one :: Natural)

instance (KnownUIntData n k, Symbolic c) => Semiring (UIntData n k c)

instance (KnownUIntData n k, Symbolic c) => Ring (UIntData n k c)

instance (KnownUIntData n k, Symbolic c) => SemiEuclidean (UIntData n k c) where
  x `divMod` y =
    toPair
    $ assert (\p@(d :*: m) ->
        isValid p && ((y == zero && d == zero) || m < y) && d * y + m == x)
    $ (\(d, m) -> fromSemiEuclidean d :*: fromSemiEuclidean m)
    $ uintDataToIntegral x `divMod` uintDataToIntegral y

instance (KnownUIntData n k, Symbolic c) => Euclidean (UIntData n k c) where
  -- | Exploits the fact that @s_i@ and @t_i@ change signs in turns on each
  -- iteration, so it adjusts the formulas correspondingly and never requires
  -- signed arithmetic. (i.e. it calculates @x = b - a@ instead of @x = a - b@
  -- when @a - b@ is negative and changes @y - x@ to @y + x@ on the following
  -- iteration) This only affects Bezout coefficients, remainders are calculated
  -- without changes as they are always non-negative.
  --
  -- If the algorithm is used to calculate Bezout coefficients, it requires that
  -- @a@ and @b@ are coprime, @b@ is not 1 and @a@ is not 0, otherwise the
  -- optimisation above is not valid.
  --
  -- If the algorithm is only used to find @gcd(a, b)@ (i.e. @s@ and @t@ will be
  -- discarded), @a@ and @b@ can be arbitrary integers.
  eea a b = eea' 1 a b one zero zero one
   where
    iterations :: Natural
    iterations = value @n * 2 + 1

    eea'
      :: Natural
      -> UIntData n k c
      -> UIntData n k c
      -> UIntData n k c
      -> UIntData n k c
      -> UIntData n k c
      -> UIntData n k c
      -> (UIntData n k c, UIntData n k c, UIntData n k c)
    eea' iteration oldR r oldS s oldT t
      | iteration == iterations = (oldS, oldT, oldR)
      | otherwise =
          ( bool recS (if even iteration then b - oldS else oldS) (r == zero)
          , bool recT (if odd iteration then a - oldT else oldT) (r == zero)
          , bool recR oldR (r == zero)
          )
     where
      quotient = oldR `div` r

      (recS, recT, recR) =
        eea'
          (iteration + 1)
          r
          (oldR - quotient * r)
          s
          (quotient * s + oldS)
          t
          (quotient * t + oldT)

resizeUIntData :: UIntData m k c -> UIntData n l c
resizeUIntData = error "TODO"

divModProp :: forall n k. (1 <= k) :- (n ~ Mod n k + Div n k * k)
divModProp =
  euclideanNat @k @n
  \\ plusCommutes @(Mod n k) @(Div n k * k)
  \\ timesCommutes @k @(Div n k)

beBSToUIntData
  :: forall n k c. (KnownUIntData n k, Symbolic c)
  => ByteString n c -> UIntData n k c
beBSToUIntData =
  (\(hi, lo) -> MkUIntData (beBSToReg hi) (beBSToReg <$> toWords lo))
  . withDict (divModProp @n @k) split

uintDataToBSbe
  :: forall n k c. (KnownUIntData n k, Symbolic c)
  => UIntData n k c -> ByteString n c
uintDataToBSbe (MkUIntData hi lo) =
  withDict (divModProp @n @k)
  $ append (regToBSbe hi)
  $ concat (regToBSbe <$> lo)

uintDataToFE :: (KnownNat k, Symbolic c) => UIntData n k c -> FieldElement c
uintDataToFE (MkUIntData hi lo) = regToFE $ appendConcatR (V.reverse lo) hi
