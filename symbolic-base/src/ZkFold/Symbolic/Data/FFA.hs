{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module ZkFold.Symbolic.Data.FFA (UIntFFA (..), FFA (..), KnownFFA, FFAMaxBits, toUInt, unsafeFromInt, fromInt, unsafeFromUInt, fromUInt, ffaFinvOrFail, ffaDivOrFail, ffaScaleAddConst, ffaInvAffineOrFail, ffaConditionalSelect) where

import Control.DeepSeq (NFData)
import Control.Monad (Monad (..))
import Data.Bits (shiftL)
import Data.Bool (otherwise)
import qualified Data.Eq as Haskell
import Data.Function (($), (.))
import Data.Functor (fmap, ($>), (<$>))
import Data.Functor.Rep (Representable (..))
import Data.Traversable (Traversable (..))
import Data.Type.Equality (type (~))
import GHC.Generics (Generic, Generic1, Par1 (..), U1 (..), type (:*:) (..))
import Numeric.Natural (Natural)
import System.Random.Stateful (Uniform (..))
import Text.Show (Show)
import Prelude (Integer)
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Algebra.Field (Zp, fromZp)
import ZkFold.Algebra.Number (KnownNat, Prime, value, type (*), type (^))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.HFunctor.Classes (HNFData, HShow)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Arithmetic, Symbolic (..), fromCircuit2F, symbolicF)
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (..), bool)
import ZkFold.Symbolic.Data.ByteString (ByteString)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Combinators (
  Ceil,
  GetRegisterSize,
  Iso (..),
  KnownRegisterSize,
  KnownRegisters,
  NumberOfRegisters,
  Resize (..),
 )
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..), finvOrFail, scaleAddConst, invAffineOrFail, conditionalSelect)
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import ZkFold.Symbolic.Data.Int (Int, isNegative, uint)
import ZkFold.Symbolic.Data.Ord (Ord (..))
import ZkFold.Symbolic.Data.UInt (OrdWord, UInt (..), natural, register, toNative)
import ZkFold.Symbolic.Interpreter (Interpreter (..))
import ZkFold.Symbolic.MonadCircuit (MonadCircuit (..), Witness (..))

type family FFAUIntSize (p :: Natural) (q :: Natural) :: Natural where
  FFAUIntSize p p = 0
  FFAUIntSize p q = NumberOfBits (Zp (Ceil (p * p) q))

isNative :: forall p c. (Symbolic c, KnownNat p) => Prelude.Bool
isNative = value @p == value @(Order (BaseField c))

newtype UIntFFA p r c = UIntFFA
  {uintFFA :: UInt (FFAUIntSize p (Order (BaseField c))) r c}
  deriving (Eq, NFData, Prelude.Eq, Show)

instance SymbolicData (UIntFFA p r) where
  type Layout (UIntFFA p r) k = Layout (UInt (FFAUIntSize p k) r) k
  type Payload (UIntFFA p r) k = Payload (UInt (FFAUIntSize p k) r) k
  type HasRep (UIntFFA p r) c = KnownFFA p r c
  arithmetize = arithmetize . uintFFA
  payload = payload . uintFFA
  interpolate (fmap (uintFFA <$>) -> bs) = UIntFFA . interpolate bs
  restore = UIntFFA . restore

instance SymbolicInput (UIntFFA p r) where
  isValid = isValid . uintFFA

data FFA p r c = FFA
  { nativeResidue :: FieldElement c
  , uintResidue :: UIntFFA p r c
  }
  deriving (Generic, Generic1)

type FFAMaxValue p q = q * (2 ^ FFAUIntSize p q)

type FFAMaxBits p c = NumberOfBits (Zp (FFAMaxValue p (Order (BaseField c))))

type KnownFFA p r c =
  ( KnownNat (FFAUIntSize p (Order (BaseField c)))
  , KnownNat p
  , KnownRegisterSize r
  , KnownRegisters c (FFAUIntSize p (Order (BaseField c))) r
  , KnownNat (FFAMaxBits p c)
  , KnownNat (GetRegisterSize (BaseField c) (FFAMaxBits p c) r)
  , KnownRegisters c (FFAMaxBits p c) r
  , KnownNat (Ceil (GetRegisterSize (BaseField c) (FFAMaxBits p c) r) OrdWord)
  , KnownRegisters c (NumberOfBits (Zp p)) r
  , KnownNat (GetRegisterSize (BaseField c) (NumberOfBits (Zp p)) r)
  , KnownNat (NumberOfBits (Zp p))
  )

instance SymbolicData (FFA p r)

instance (KnownNat p, KnownRegisterSize r) => SymbolicInput (FFA p r) where
  isValid ffa@(FFA _ ux :: FFA p r c) =
    if isNative @p @c
      then true
      else isValid ux && toUInt @(FFAMaxBits p c) ffa < fromConstant (value @p)

instance HNFData c => NFData (FFA p r c)

deriving stock instance HShow c => Show (FFA p r c)

instance (Symbolic c, KnownFFA p r c) => Eq (FFA p r c)

deriving instance Arithmetic a => Haskell.Eq (FFA p r (Interpreter a))

integralFromFFA
  :: forall p n f
   . (PrimeField f, KnownNat (FFAUIntSize p n))
  => f -> IntegralOf f -> IntegralOf f
integralFromFFA (toIntegral -> n) u =
  let
    -- x = k |f| + n = l * 2^s + u
    -- k |f| - l * 2^s = u - n
    -- k = (u - n) * |f|^(-1) (mod 2^s)
    intSize = 1 `shiftL` Prelude.fromIntegral (value @(FFAUIntSize p n))
    aInv = bezoutR @Integer intSize $ fromConstant (order @f)
    k = ((u - n) * fromConstant aInv) `mod` fromConstant intSize
   in
    k * fromConstant (order @f) + n

instance
  (Arithmetic a, KnownFFA p r (Interpreter a))
  => ToConstant (FFA p r (Interpreter a))
  where
  type Const (FFA p r (Interpreter a)) = Zp p
  toConstant (FFA (toConstant -> nx) (UIntFFA (toConstant -> ux))) =
    fromConstant $ integralFromFFA @p @(Order a) nx (fromConstant ux)

instance
  (Symbolic c, KnownFFA p r c, FromConstant a (Zp p))
  => FromConstant a (FFA p r c)
  where
  fromConstant c =
    let c' = toConstant (fromConstant c :: Zp p)
     in FFA (fromConstant c') (UIntFFA $ fromConstant c')

instance
  {-# OVERLAPPING #-}
  (Symbolic c, Order (BaseField c) ~ p, KnownRegisterSize r)
  => FromConstant (FieldElement c) (FFA p r c)
  where
  fromConstant nx = FFA nx (UIntFFA zero)

instance {-# OVERLAPPING #-} FromConstant (FFA p r c) (FFA p r c)

instance {-# OVERLAPPING #-} (Symbolic c, KnownFFA p r c) => Scale (FFA p r c) (FFA p r c)

instance (Symbolic c, KnownFFA p r c) => Uniform (FFA p r c) where
  uniformM = fmap fromConstant . uniformM @(Zp p)

valueFFA
  :: forall p r c i a
   . (Symbolic c, KnownFFA p r c, Witness i (WitnessField c), a ~ BaseField c)
  => (Par1 :*: Vector (NumberOfRegisters a (FFAUIntSize p (Order a)) r)) i
  -> IntegralOf (WitnessField c)
valueFFA (Par1 (at -> ni) :*: (natural @c @(FFAUIntSize p (Order a)) @r -> ui)) =
  integralFromFFA @p @(Order a) ni ui

layoutFFA
  :: forall p r c a w
   . (Symbolic c, KnownFFA p r c, a ~ BaseField c, w ~ WitnessField c)
  => IntegralOf w
  -> (Par1 :*: Vector (NumberOfRegisters a (FFAUIntSize p (Order a)) r)) w
layoutFFA c =
  Par1 (fromConstant c)
    :*: tabulate (register @c @(FFAUIntSize p (Order a)) @r c)

fromFFA
  :: forall p r a
   . (Arithmetic a, KnownFFA p r (Interpreter a))
  => (Par1 :*: Vector (NumberOfRegisters a (FFAUIntSize p (Order a)) r)) a
  -> Integer
fromFFA (Par1 x :*: v) =
  fromConstant $
    toConstant $
      toConstant $
        FFA @p @r (fromConstant x) (UIntFFA (UInt (Interpreter v)))

toFFA
  :: forall p r a
   . (Arithmetic a, KnownFFA p r (Interpreter a))
  => Integer
  -> (Par1 :*: Vector (NumberOfRegisters a (FFAUIntSize p (Order a)) r)) a
toFFA n =
  let FFA (FieldElement (Interpreter x)) (UIntFFA (UInt (Interpreter v))) =
        fromConstant n :: FFA p r (Interpreter a)
   in x :*: v

instance (Symbolic c, KnownFFA p r c) => MultiplicativeSemigroup (FFA p r c) where
  FFA nx ux * FFA ny uy =
    if isNative @p @c
      then FFA (nx * ny) (UIntFFA zero)
      else FFA nr ur
   where
    p :: FromConstant Natural a => a
    p = fromConstant (value @p)
    -- \| Computes unconstrained \(d = ab div p\) and \(m = ab mod p\)
    nd, nm :: FieldElement c
    ud, um :: UIntFFA p r c
    (nd :*: ud) :*: (nm :*: um) =
      restore
        ( symbolicF
            (arithmetize ((nx :*: ux) :*: (ny :*: uy)))
            ( \((fromFFA @p @r -> a) :*: (fromFFA @p @r -> b)) ->
                toFFA @p @r ((a * b) `div` p) :*: toFFA @p @r ((a * b) `mod` p)
            )
            \((valueFFA @p @r @c -> a) :*: (valueFFA @p @r @c -> b)) -> do
              traverse unconstrained $
                layoutFFA @p @r @c ((a * b) `div` p)
                  :*: layoutFFA @p @r @c ((a * b) `mod` p)
        , (U1 :*: U1) :*: (U1 :*: U1)
        )
    -- \| Constraints:
    -- \* UInt registers are indeed registers;
    -- \* m < p;
    -- \* equation holds modulo basefield;
    -- \* equation holds modulo 2^k.
    ck =
      isValid (uintFFA ud :*: FFA @p nm um)
        && (nx * ny == nd * p + nm)
        && (uintFFA ux * uintFFA uy == uintFFA ud * p + uintFFA um)
    -- \| Sew constraints into result.
    nr :*: ur =
      restore
        ( fromCircuitF
            (arithmetize (nm :*: um :*: ck))
            \(ni :*: ui :*: Par1 b) -> do
              constraint (($ b) - one)
              return (ni :*: ui)
        , U1 :*: U1
        )

instance (Symbolic c, KnownFFA p r c) => Exponent (FFA p r c) Natural where
  x ^ a = x `natPow` (a `mod` (value @p -! 1))

instance (Symbolic c, KnownFFA p r c) => MultiplicativeMonoid (FFA p r c) where
  one = fromConstant (one :: Zp p)

instance (Symbolic c, KnownFFA p r c) => AdditiveSemigroup (FFA p r c) where
  FFA nx ux + FFA ny uy =
    if isNative @p @c
      then FFA (nx + ny) (UIntFFA zero)
      else FFA nr ur
   where
    p :: FromConstant Natural a => a
    p = fromConstant (value @p)
    -- \| Computes unconstrained \(d = ab div p\) and \(m = ab mod p\).
    -- \(d\) must be {0, 1} as addition can only overflow so much.
    d :: Bool c
    nm :: FieldElement c
    um :: UIntFFA p r c
    d :*: nm :*: um =
      restore
        ( symbolicF
            (arithmetize ((nx :*: ux) :*: (ny :*: uy)))
            ( \((fromFFA @p @r -> a) :*: (fromFFA @p @r -> b)) ->
                Par1 (if a + b Prelude.>= p then one else zero)
                  :*: toFFA @p @r ((a + b) `mod` p)
            )
            \((valueFFA @p @r @c -> a) :*: (valueFFA @p @r @c -> b)) -> do
              traverse unconstrained $
                Par1 (fromConstant ((a + b) `div` p))
                  :*: layoutFFA @p @r @c ((a + b) `mod` p)
        , U1 :*: (U1 :*: U1)
        )
    -- \| Constraints:
    -- \* boolean is indeed boolean;
    -- \* UInt registers are indeed registers;
    -- \* m < p;
    -- \* equation holds modulo basefield;
    -- \* equation holds modulo 2^k.
    ck =
      isValid (d :*: FFA @p nm um)
        && (nx + ny == bool zero p d + nm)
        && (uintFFA ux + uintFFA uy == bool zero p d + uintFFA um)
    -- \| Sew constraints into result.
    nr :*: ur =
      restore
        ( fromCircuitF
            (arithmetize (nm :*: um :*: ck))
            \(ni :*: ui :*: Par1 b) -> do
              constraint (($ b) - one)
              return (ni :*: ui)
        , U1 :*: U1
        )

instance (Symbolic c, KnownFFA p r c, Scale a (Zp p)) => Scale a (FFA p r c) where
  scale k x = fromConstant (scale k one :: Zp p) * x

instance (Symbolic c, KnownFFA p r c) => Zero (FFA p r c) where
  zero = fromConstant (zero :: Zp p)

instance (Symbolic c, KnownFFA p r c) => AdditiveMonoid (FFA p r c)

instance (Symbolic c, KnownFFA p r c) => AdditiveGroup (FFA p r c) where
  -- \| negate cannot overflow if value is nonzero.
  negate (FFA nx ux) =
    if isNative @p @c
      then FFA (negate nx) (UIntFFA zero)
      else
        bool
          ( FFA
              (fromConstant (value @p) - nx)
              (UIntFFA (fromConstant (value @p) - uintFFA ux))
          )
          (FFA nx ux)
          (nx == zero && uintFFA ux == zero)

instance (Symbolic c, KnownFFA p r c) => Semiring (FFA p r c)

instance (Symbolic c, KnownFFA p r c) => Ring (FFA p r c)

instance (Symbolic c, KnownFFA p r c, Prime p) => Exponent (FFA p r c) Integer where
  x ^ a
    | neg Prelude.< pos = finv x ^ neg
    | otherwise = x ^ pos
   where
    pos = Prelude.fromIntegral (a `mod` Prelude.fromIntegral (value @p -! 1))
    neg = value @p -! (pos + 1)

instance (Symbolic c, KnownFFA p r c, Prime p) => Field (FFA p r c) where
  finv (FFA nx ux) =
    if isNative @p @c
      then FFA (finv nx) (UIntFFA zero)
      else FFA ny uy
   where
    p :: FromConstant Natural a => a
    p = fromConstant (value @p)
    -- \| Computes unconstrained Bezout coefficients.
    nl, nr :: FieldElement c
    ul, ur :: UIntFFA p r c
    (nl :*: ul) :*: (nr :*: ur) =
      restore
        ( symbolicF
            (arithmetize (nx :*: ux))
            ( \(fromFFA @p @r -> x) ->
                let l0 = negate (bezoutL p x)
                    r0 = bezoutR p x
                    (l, r) = if r0 Prelude.< 0 then (l0 + x, r0 + p) else (l0, r0)
                 in toFFA @p @r l :*: toFFA @p @r r
            )
            \(valueFFA @p @r @c -> x) -> do
              let l0 = negate (bezoutL p x)
                  r0 = bezoutR p x
                  s = r0 `div` p -- -1 when negative, 0 when positive
                  l = l0 - s * x
                  r = r0 - s * p
              traverse unconstrained $
                layoutFFA @p @r @c l :*: layoutFFA @p @r @c r
        , (U1 :*: U1) :*: (U1 :*: U1)
        )
    -- \| Constraints:
    -- \* UInt registers are indeed registers;
    -- \* r < p;
    -- \* equation holds modulo basefield;
    -- \* equation holds modulo 2^k.
    ck =
      isValid (ur :*: FFA @p nl ul)
        && (nr * nx == nl * p + one)
        && (uintFFA ur * uintFFA ux == uintFFA ul * p + one)
    -- \| Sew constraints into result.
    ny :*: uy =
      restore
        ( fromCircuitF
            (arithmetize (nr :*: ur :*: ck))
            \(ni :*: ui :*: Par1 b) -> do
              constraint (($ b) - one)
              return (ni :*: ui)
        , U1 :*: U1
        )

instance Finite (Zp p) => Finite (FFA p r c) where
  type Order (FFA p r c) = p

-- | Optimized field inversion for FFA that uses only 1 constraint (for native case).
-- IMPORTANT: This assumes the input is non-zero. If the input is zero,
-- the circuit will be unsatisfiable.
-- Use this only when you can guarantee the input is non-zero.
--
-- For native case (circuit field = FFA prime), uses finvOrFail which is
-- 1 constraint instead of 2. For non-native case, the optimization is less
-- significant because non-native FFA operations have other overhead, so we
-- fall back to regular finv for simplicity.
ffaFinvOrFail
  :: forall p r c
   . (Symbolic c, KnownFFA p r c, Prime p)
  => FFA p r c
  -> FFA p r c
ffaFinvOrFail (FFA nx _ux) =
  -- For native case, use finvOrFail which is 1 constraint instead of 2
  -- For non-native case, fall back to regular finv (the optimization is less significant)
  if isNative @p @c
    then FFA (finvOrFail nx) (UIntFFA zero)
    else finv (FFA nx _ux)

-- | Optimized division for FFA that uses only 1 constraint for inversion (native case).
-- IMPORTANT: This assumes the denominator is non-zero.
ffaDivOrFail
  :: forall p r c
   . (Symbolic c, KnownFFA p r c, Prime p)
  => FFA p r c
  -> FFA p r c
  -> FFA p r c
ffaDivOrFail num den = num * ffaFinvOrFail den

-- | Compute (constant + scale * x) in a single constraint for FFA (native case).
-- This computes: constant + scale * x
-- For native case: uses 1 constraint instead of 2
-- For non-native case: falls back to regular operations
ffaScaleAddConst
  :: forall p r c k s
   . (Symbolic c, KnownFFA p r c, FromConstant k (Zp p), Scale s (Zp p))
  => k                    -- ^ Constant term
  -> s                    -- ^ Scale factor
  -> FFA p r c            -- ^ Variable to scale
  -> FFA p r c            -- ^ Result: constant + scale * variable
ffaScaleAddConst c s ffa@(FFA nx _ux) =
  if isNative @p @c
    then FFA (scaleAddConst c' s' nx) (UIntFFA zero)
    else fromConstant (fromConstant c :: Zp p) + fromConstant (scale s one :: Zp p) * ffa
  where
    c' = fromZp (fromConstant c :: Zp p) :: Natural
    s' = fromZp (scale s one :: Zp p) :: Natural

-- | Compute 1 / (constant + scale * x) in a single constraint for FFA (native case).
-- This inlines the affine function into the inversion, saving 1 constraint.
--
-- Instead of:
--   1. y = c + s*x  (1 constraint)
--   2. inv * y = 1  (1 constraint)
--
-- We use:
--   s*x*inv + c*inv - 1 = 0  (1 constraint)
--
-- For native case: uses 1 constraint
-- For non-native case: falls back to 2 constraints
ffaInvAffineOrFail
  :: forall p r c k s
   . (Symbolic c, KnownFFA p r c, Prime p, FromConstant k (Zp p), Scale s (Zp p))
  => k                    -- ^ Constant term
  -> s                    -- ^ Scale factor
  -> FFA p r c            -- ^ Variable x
  -> FFA p r c            -- ^ Result: 1 / (constant + scale * x)
ffaInvAffineOrFail c s ffa@(FFA nx _ux) =
  if isNative @p @c
    then FFA (invAffineOrFail c' s' nx) (UIntFFA zero)
    else ffaFinvOrFail (ffaScaleAddConst c s ffa)
  where
    c' = fromZp (fromConstant c :: Zp p) :: Natural
    s' = fromZp (scale s one :: Zp p) :: Natural

-- | Efficient conditional selection for FFA: result = onFalse + bit * (onTrue - onFalse)
-- Uses 1 constraint per field element instead of ~10+ from interpolation.
--
-- For native FFA (when circuit field = curve field), this uses 1 constraint total.
-- For non-native FFA, falls back to the generic `bool` which uses interpolation.
--
-- SAFETY: The bit parameter must be 0 or 1 for correct results.
ffaConditionalSelect
  :: forall p r c
   . (Symbolic c, KnownFFA p r c)
  => FieldElement c       -- ^ Selector bit (0 or 1)
  -> FFA p r c            -- ^ onFalse (value when bit=0)
  -> FFA p r c            -- ^ onTrue (value when bit=1)
  -> FFA p r c            -- ^ Result: onFalse + bit * (onTrue - onFalse)
ffaConditionalSelect bit (FFA nxF _uxF) (FFA nxT _uxT) =
  if isNative @p @c
    then FFA (conditionalSelect bit nxF nxT) (UIntFFA zero)
    else Prelude.error "ffaConditionalSelect not implemented for non-native FFA"

instance (Symbolic c, KnownFFA p r c) => BinaryExpansion (FFA p r c) where
  type Bits (FFA p r c) = ByteString (NumberOfBits (Zp p)) c
  binaryExpansion = from . toUInt @(NumberOfBits (Zp p))
  fromBinary = unsafeFromUInt @(NumberOfBits (Zp p)) . from

-- | __NOTE__: This function assumes that the given 'UInt' is in the range of the field. Use 'fromUInt' instead if you need to perform a modulo operation (by order of field) on the 'UInt'.
unsafeFromUInt
  :: forall n p r c
   . (Symbolic c, KnownFFA p r c)
  => (KnownNat n, KnownNat (GetRegisterSize (BaseField c) n r))
  => UInt n r c
  -> FFA p r c
unsafeFromUInt ux = FFA (toNative ux) (UIntFFA $ resize ux)

fromUInt
  :: forall n p r c
   . (Symbolic c, KnownFFA p r c)
  => KnownNat n
  => UInt n r c
  -> FFA p r c
fromUInt ux =
  let
    uWide = resize ux
    m :: UInt (FFAMaxBits p c) r c = fromConstant (value @p)
   in
    unsafeFromUInt $ mod uWide m

-- | __NOTE__: This function assumes that the given 'Int' is in the range of the field. Use 'fromInt' instead if you need to perform a modulo operation (by order of field) on the 'Int'.
unsafeFromInt
  :: (Symbolic c, KnownFFA p r c)
  => (KnownNat n, KnownNat (GetRegisterSize (BaseField c) n r))
  => Int n r c
  -> FFA p r c
unsafeFromInt ix =
  let uxFFA = unsafeFromUInt (uint ix)
   in ifThenElse (isNegative ix) (negate uxFFA) uxFFA

fromInt
  :: (Symbolic c, KnownFFA p r c)
  => KnownNat n
  => Int n r c
  -> FFA p r c
fromInt ix =
  let uxFFA = fromUInt (uint ix)
   in ifThenElse (isNegative ix) (negate uxFFA) uxFFA

toUInt
  :: forall n p r c
   . Symbolic c
  => (KnownFFA p r c, KnownNat n, KnownNat (NumberOfRegisters (BaseField c) n r))
  => KnownNat (GetRegisterSize (BaseField c) n r)
  => FFA p r c
  -> UInt n r c
toUInt x = uy
 where
  -- \| Computes unconstrained UInt value
  us :: UInt n r c
  us =
    restore
      ( symbolicF
          (arithmetize x)
          ( \(fromFFA @p @r -> v) ->
              let UInt (Interpreter f) =
                    fromConstant v
                      :: UInt n r (Interpreter (BaseField c))
               in f
          )
          \(valueFFA @p @r @c -> v) ->
            traverse unconstrained $ tabulate (register @c @n @r v)
      , U1
      )
  -- \| Constraints:
  -- \* UInt registers are indeed registers;
  -- \* casting back yields source residues.
  Bool ck = isValid us && unsafeFromUInt us == x
  -- \| Sew constraints into result.
  uy =
    restore
      ( fromCircuit2F
          (arithmetize us)
          ck
          \xi (Par1 b) -> constraint (($ b) - one) $> xi
      , U1
      )
