{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module ZkFold.Symbolic.Data.FFA (UIntFFA (..), FFA (..), KnownFFA, FFAMaxBits, toUInt, unsafeFromInt, fromInt, unsafeFromUInt, fromUInt) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL)
import Data.Bool (otherwise)
import qualified Data.Eq as Haskell
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Functor.Rep (Representable (..))
import Data.Type.Equality (type (~))
import GHC.Generics (Generic, Generic1, Par1 (..), type (:*:) (..))
import Numeric.Natural (Natural)
import System.Random.Stateful (Uniform (..))
import Text.Show (Show)
import Prelude (Integer)
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number (KnownNat, Prime, value, type (*), type (^))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Compat (CompatContext (..), CompatData (CompatData, compatData), assert)
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (..), bool)
import ZkFold.Symbolic.Data.ByteString (ByteString)
import ZkFold.Symbolic.Data.Combinators (
  Ceil,
  GetRegisterSize,
  Iso (..),
  KnownRegisterSize,
  KnownRegisters,
  NumberOfRegisters,
  Resize (..),
 )
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.Input (isValid)
import ZkFold.Symbolic.Data.Int (Int, isNegative, uint)
import ZkFold.Symbolic.Data.Ord (Ord (..))
import ZkFold.Symbolic.Data.UInt
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import ZkFold.Symbolic.Data.V2 (SymbolicData (..))
import ZkFold.Symbolic.V2 (Symbolic)
import GHC.Err (error)

type family FFAUIntSize (p :: Natural) (q :: Natural) :: Natural where
  FFAUIntSize p p = 0
  FFAUIntSize p q = NumberOfBits (Zp (Ceil (p * p) q))

isNative :: forall p c. (Symbolic c, KnownNat p) => Prelude.Bool
isNative = value @p == value @(Order c)

newtype UIntFFA p r c = UIntFFA {uintFFA :: UInt (FFAUIntSize p (Order c)) r c}
  deriving (Eq, Generic, NFData, Prelude.Eq, Show)

deriving newtype instance
  (Symbolic c, KnownFFA p r c) => AdditiveSemigroup (UIntFFA p r c)

deriving newtype instance
  (Symbolic c, KnownFFA p r c) => MultiplicativeSemigroup (UIntFFA p r c)

deriving newtype instance
  (Symbolic c, KnownFFA p r c) => MultiplicativeMonoid (UIntFFA p r c)

deriving newtype instance
  (Symbolic c, KnownFFA p r c) => FromConstant Natural (UIntFFA p r c)

instance (Symbolic c, KnownFFA p r c) => Exponent (UIntFFA p r c) Natural where
  UIntFFA x ^ e = UIntFFA (x ^ e)

instance SymbolicData (UIntFFA p r) where
  type Layout (UIntFFA p r) c = Layout (UInt (FFAUIntSize p (Order c)) r) c
  type HasRep (UIntFFA p r) c = KnownFFA p r c
  toLayout = toLayout . uintFFA
  interpolate b = UIntFFA . interpolate b . fmap (uintFFA <$>)
  fromLayout = UIntFFA . fromLayout

instance Symbolic c => Collect (ConstrainedDatum c) (UIntFFA p r c)

isUIntFFAValid
  :: (Symbolic c, KnownFFA p r c) => UIntFFA p r c -> CompatData Bool c
isUIntFFAValid (UIntFFA _u) = error "TODO"

data FFA p r c = FFA
  { nativeResidue :: CompatData FieldElement c
  , uintResidue :: UIntFFA p r c
  }
  deriving (Generic, Generic1)

type FFAMaxValue p q = q * (2 ^ FFAUIntSize p q)

type FFAMaxBits p c = NumberOfBits (Zp (FFAMaxValue p (Order c)))

type KnownFFA p r c =
  ( KnownNat (FFAUIntSize p (Order c))
  , KnownNat p
  , KnownRegisterSize r
  , KnownRegisters c (FFAUIntSize p (Order c)) r
  , KnownNat (FFAMaxBits p c)
  , KnownNat (GetRegisterSize c (FFAMaxBits p c) r)
  , KnownRegisters c (FFAMaxBits p c) r
  , KnownNat (Ceil (GetRegisterSize c (FFAMaxBits p c) r) OrdWord)
  , KnownRegisters c (NumberOfBits (Zp p)) r
  , KnownNat (GetRegisterSize c (NumberOfBits (Zp p)) r)
  , KnownNat (NumberOfBits (Zp p))
  )

instance SymbolicData (FFA p r)

instance Symbolic c => Collect (ConstrainedDatum c) (FFA p r c)

-- TODO: Restore SymbolicInput class after refactoring of Bool
isValidFFA
  :: forall p r c. (Symbolic c, KnownFFA p r c) => FFA p r c -> CompatData Bool c
isValidFFA ffa@(FFA _ u) =
  isUIntFFAValid u && toUInt @(FFAMaxBits p c) ffa < fromConstant (value @p)

instance NFData c => NFData (FFA p r c)

deriving stock instance Show c => Show (FFA p r c)

instance (Symbolic c, KnownFFA p r c) => Eq (FFA p r c)

deriving instance Arithmetic a => Haskell.Eq (FFA p r a)

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

instance (Arithmetic a, KnownFFA p r a) => ToConstant (FFA p r a) where
  type Const (FFA p r a) = Zp p
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
  (Symbolic c, Order c ~ p, KnownRegisterSize r)
  => FromConstant (CompatData FieldElement c) (FFA p r c)
  where
  fromConstant nx = FFA nx (UIntFFA zero)

instance {-# OVERLAPPING #-} FromConstant (FFA p r c) (FFA p r c)

instance {-# OVERLAPPING #-} (Symbolic c, KnownFFA p r c) => Scale (FFA p r c) (FFA p r c)

instance (Symbolic c, KnownFFA p r c) => Uniform (FFA p r c) where
  uniformM = fmap fromConstant . uniformM @(Zp p)

valueFFA
  :: forall p r c
   . (Symbolic c, KnownFFA p r c)
  => CompatData FieldElement c -> UIntFFA p r c -> IntegralOf c
valueFFA
  (toLayout -> Par1 ni :*: _)
  (natural @c @c @(FFAUIntSize p (Order c)) @r . toLayout -> ui) =
    integralFromFFA @p @(Order c) ni ui

layoutUInt
  :: forall n r c
   . (PrimeField c, KnownNat n, KnownRegisterSize r, KnownRegisters c n r)
  => IntegralOf c -> UInt n r c
layoutUInt = UInt . tabulate . register @_ @n @r @c

layoutFFA
  :: forall p r c. (Symbolic c, KnownFFA p r c) => IntegralOf c -> FFA p r c
layoutFFA c =
  FFA
    (CompatData $ FieldElement $ CompatContext $ Par1 $ fromConstant c)
    (UIntFFA $ layoutUInt c)

toBool :: Field c => BooleanOf c -> CompatData Bool c
toBool = CompatData . Bool . CompatContext . Par1 . bool zero one

instance (Symbolic c, KnownFFA p r c) => MultiplicativeSemigroup (FFA p r c) where
  FFA nx ux * FFA ny uy =
    if isNative @p @c
      then FFA (nx * ny) (UIntFFA zero)
      else result
   where
    p :: FromConstant Natural a => a
    p = fromConstant (value @p)
    -- Compute unconstrained \(d = ab div p\) and \(m = ab mod p\)
    (di, mi) = (valueFFA @p @r nx ux * valueFFA @p @r ny uy) `divMod` p
    FFA nd ud = layoutFFA @p @r di
    -- Constrain the result
    result =
      assert
        ( \ffa@(FFA nm um) ->
            isUIntFFAValid ud -- UInt is indeed UInt
              && isValidFFA ffa -- result is indeed FFA
              && (nx * ny == nd * p + nm) -- equation holds modulo basefield
              && (ux * uy == ud * p + um) -- equation holds modulo 2^k.
        )
        (layoutFFA mi)

instance (Symbolic c, KnownFFA p r c) => Exponent (FFA p r c) Natural where
  x ^ a = x `natPow` (a `mod` (value @p -! 1))

instance (Symbolic c, KnownFFA p r c) => MultiplicativeMonoid (FFA p r c) where
  one = fromConstant (one :: Zp p)

instance (Symbolic c, KnownFFA p r c) => AdditiveSemigroup (FFA p r c) where
  FFA nx ux + FFA ny uy =
    if isNative @p @c
      then FFA (nx + ny) (UIntFFA zero)
      else result
   where
    p :: FromConstant Natural a => a
    p = fromConstant (value @p)
    -- Computes unconstrained \(d = (a+b) div p\) and \(m = (a+b) mod p\).
    -- \(d\) must be {0, 1} as addition can only overflow so much.
    a = valueFFA @p @r nx ux
    b = valueFFA @p @r ny uy
    d = toBool @c ((a + b) >= p)
    -- Constrain the result
    result =
      assert
        ( \ffa@(FFA nm um) ->
            CompatData (isValid $ compatData d) -- boolean is indeed boolean;
              && isValidFFA ffa -- result is indeed FFA;
              && (nx + ny == bool nm (nm + p) d) -- equation holds modulo basefield;
              && (ux + uy == bool um (um + p) d) -- equation holds modulo 2^k.
        )
        $ layoutFFA ((a + b) `mod` p)

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
      else result
   where
    p :: FromConstant Natural a => a
    p = fromConstant (value @p)
    -- Compute unconstrained Bezout coefficients
    x = valueFFA @p @r nx ux
    l0 = negate (bezoutL p x)
    r0 = bezoutR p x
    sign = r0 < zero
    FFA nl ul = layoutFFA @p @r (ifThenElse sign (l0 + x) l0)
    -- Constrain the result
    result =
      assert
        ( \ffa@(FFA nr ur) ->
            isValidFFA ffa -- result is indeed FFA
              && isUIntFFAValid ul -- UInt is indeed UInt
              && (nr * nx == nl * p + one) -- equation holds modulo basefield
              && (ur * ux == ul * p + one) -- equation holds modulo 2^k
        )
        $ layoutFFA (ifThenElse sign (r0 + p) r0)

instance Finite (Zp p) => Finite (FFA p r c) where
  type Order (FFA p r c) = p

instance (Symbolic c, KnownFFA p r c) => BinaryExpansion (FFA p r c) where
  type Bits (FFA p r c) = ByteString (NumberOfBits (Zp p)) c
  binaryExpansion = from . toUInt @(NumberOfBits (Zp p))
  fromBinary = unsafeFromUInt @(NumberOfBits (Zp p)) . from

-- | __NOTE__: This function assumes that the given 'UInt' is in the range of the field. Use 'fromUInt' instead if you need to perform a modulo operation (by order of field) on the 'UInt'.
unsafeFromUInt
  :: forall n p r c
   . (Symbolic c, KnownFFA p r c)
  => (KnownNat n, KnownNat (GetRegisterSize c n r))
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
  => (KnownNat n, KnownNat (GetRegisterSize c n r))
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
  => (KnownFFA p r c, KnownNat n, KnownNat (NumberOfRegisters c n r))
  => KnownNat (GetRegisterSize c n r)
  => FFA p r c
  -> UInt n r c
toUInt src@(FFA nx ux) =
  assert
    (\res -> isValidUInt res && unsafeFromUInt res == src)
    (layoutUInt (valueFFA @p @r nx ux))
