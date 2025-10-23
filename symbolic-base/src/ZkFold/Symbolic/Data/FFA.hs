{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module ZkFold.Symbolic.Data.FFA (
  UIntFFA (..),
  FFA (..),
  KnownFFA,
  FFAMaxBits,
  toUInt,
  unsafeFromInt,
  fromInt,
  unsafeFromUInt,
  fromUInt,
) where

import Data.Bits (shiftL)
import Data.Bool (otherwise)
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Type.Equality (type (~))
import GHC.Generics (Generic, Generic1, Par1 (..), type (:*:) (..))
import System.Random.Stateful (Uniform (..))
import Test.QuickCheck (Arbitrary (arbitrary))
import Text.Show (Show)
import Prelude (Integer)
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Symbolic.Class (Arithmetic, Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (..), assert, bool)
import ZkFold.Symbolic.Data.ByteString (ByteString)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput, isValid)
import ZkFold.Symbolic.Data.Int (Int (intToUInt), isNegative)
import ZkFold.Symbolic.Data.Ord ((<), (>=))
import ZkFold.Symbolic.Data.UInt
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)

type family FFAUIntSize (p :: Natural) (q :: Natural) :: Natural where
  FFAUIntSize p p = 0
  FFAUIntSize p q = NumberOfBits (Zp (Div (p * p + q - 1) q))

isNative :: forall p c. (Symbolic c, KnownNat p) => Prelude.Bool
isNative = value @p == value @(Order c)

newtype UIntFFA p c = UIntFFA {uintFFA :: UInt (FFAUIntSize p (Order c)) c}
  deriving (Eq, Generic, Show)

deriving newtype instance (Symbolic c, KnownFFA p c) => Arbitrary (UIntFFA p c)

deriving newtype instance
  (Symbolic c, KnownFFA p c) => AdditiveSemigroup (UIntFFA p c)

deriving newtype instance
  (Symbolic c, KnownFFA p c) => MultiplicativeSemigroup (UIntFFA p c)

deriving newtype instance
  (Symbolic c, KnownFFA p c) => MultiplicativeMonoid (UIntFFA p c)

deriving newtype instance
  (Symbolic c, KnownFFA p c) => FromConstant Natural (UIntFFA p c)

instance (Symbolic c, KnownFFA p c) => Exponent (UIntFFA p c) Natural where
  UIntFFA x ^ e = UIntFFA (x ^ e)

instance SymbolicData (UIntFFA p) where
  type Layout (UIntFFA p) c = Layout (UInt (FFAUIntSize p (Order c))) c
  type HasRep (UIntFFA p) c = KnownFFA p c
  toLayout = toLayout . uintFFA
  interpolate b = UIntFFA . interpolate b . fmap (uintFFA <$>)
  fromLayout = UIntFFA . fromLayout

instance SymbolicInput (UIntFFA p) where
  isValid = isValid . uintFFA

instance Symbolic c => Collect (ConstrainedDatum c) (UIntFFA p c)

data FFA p c = FFA
  { nativeResidue :: FieldElement c
  , uintResidue :: UIntFFA p c
  }
  deriving (Generic, Generic1, Show)

type FFAMaxValue p q = q * (2 ^ FFAUIntSize p q)

type FFAMaxBits p c = NumberOfBits (Zp (FFAMaxValue p (Order c)))

type KnownFFA p c =
  ( KnownNat p
  , KnownUInt (FFAUIntSize p (Order c)) c
  , KnownUInt (FFAMaxBits p c) c
  , KnownUInt (NumberOfBits (Zp p)) c
  )

instance SymbolicData (FFA p)

instance SymbolicInput (FFA p) where
  isValid ffa@(FFA _ u :: FFA p c) =
    isValid u && toUInt @(FFAMaxBits p c) ffa < fromConstant (value @p)

instance Symbolic c => Collect (ConstrainedDatum c) (FFA p c)

instance (Symbolic c, KnownFFA p c) => Arbitrary (FFA p c) where
  arbitrary = fromConstant <$> arbitrary @(Zp p)

instance (Symbolic c, KnownFFA p c) => Eq (FFA p c)

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

instance (Arithmetic a, KnownFFA p a) => ToConstant (FFA p a) where
  type Const (FFA p a) = Zp p
  toConstant
    ( FFA
        (toConstant -> nx)
        (UIntFFA (toConstant . toConstant -> ux))
      ) =
      fromConstant $ integralFromFFA @p @(Order a) nx (fromConstant ux)

instance
  (Symbolic c, KnownFFA p c, FromConstant a (Zp p))
  => FromConstant a (FFA p c)
  where
  fromConstant c =
    let c' = toConstant (fromConstant c :: Zp p)
     in FFA (fromConstant c') (UIntFFA $ fromConstant c')

instance
  {-# OVERLAPPING #-}
  (Symbolic c, Order c ~ p, KnownUInt (FFAUIntSize p (Order c)) c)
  => FromConstant (FieldElement c) (FFA p c)
  where
  fromConstant nx = FFA nx (UIntFFA zero)

instance {-# OVERLAPPING #-} FromConstant (FFA p c) (FFA p c)

instance {-# OVERLAPPING #-} (Symbolic c, KnownFFA p c) => Scale (FFA p c) (FFA p c)

instance (Symbolic c, KnownFFA p c) => Uniform (FFA p c) where
  uniformM = fmap fromConstant . uniformM @(Zp p)

valueFFA
  :: forall p c
   . (Symbolic c, KnownFFA p c)
  => FieldElement c -> UIntFFA p c -> IntegralOf c
valueFFA (toLayout -> Par1 ni) (uintToIntegral . uintFFA -> ui) =
  integralFromFFA @p @(Order c) ni ui

layoutFFA :: forall p c. (Symbolic c, KnownFFA p c) => IntegralOf c -> FFA p c
layoutFFA c =
  FFA (FieldElement $ fromConstant c) (UIntFFA $ uintFromSemiEuclidean c)

toBool :: Field c => BooleanOf c -> Bool c
toBool = Bool . bool zero one

instance (Symbolic c, KnownFFA p c) => MultiplicativeSemigroup (FFA p c) where
  FFA nx ux * FFA ny uy =
    if isNative @p @c
      then FFA (nx * ny) (UIntFFA zero)
      else result
   where
    p :: FromConstant Natural a => a
    p = fromConstant (value @p)
    -- Compute unconstrained \(d = ab div p\) and \(m = ab mod p\)
    (di, mi) = (valueFFA @p nx ux * valueFFA @p ny uy) `divMod` p
    FFA nd ud = layoutFFA @p di
    -- Constrain the result
    result =
      assert
        ( \ffa@(FFA nm um) ->
            isValid (ud :*: ffa) -- UInt is indeed UInt, result is indeed FFA
              && (nx * ny == nd * p + nm) -- equation holds modulo basefield
              && (ux * uy == ud * p + um) -- equation holds modulo 2^k.
        )
        (layoutFFA mi)

instance (Symbolic c, KnownFFA p c) => Exponent (FFA p c) Natural where
  x ^ a = x `natPow` (a `mod` (value @p -! 1))

instance (Symbolic c, KnownFFA p c) => MultiplicativeMonoid (FFA p c) where
  one = fromConstant (one :: Zp p)

instance (Symbolic c, KnownFFA p c) => AdditiveSemigroup (FFA p c) where
  FFA nx ux + FFA ny uy =
    if isNative @p @c
      then FFA (nx + ny) (UIntFFA zero)
      else result
   where
    p :: FromConstant Natural a => a
    p = fromConstant (value @p)
    -- Computes unconstrained \(d = (a+b) div p\) and \(m = (a+b) mod p\).
    -- \(d\) must be {0, 1} as addition can only overflow so much.
    a = valueFFA @p nx ux
    b = valueFFA @p ny uy
    d = toBool @c ((a + b) >= p)
    -- Constrain the result
    result =
      assert
        ( \ffa@(FFA nm um) ->
            isValid (d :*: ffa) -- boolean is indeed boolean, result is indeed FFA;
              && (nx + ny == bool nm (nm + p) d) -- equation holds modulo basefield;
              && (ux + uy == bool um (um + p) d) -- equation holds modulo 2^k.
        )
        $ layoutFFA ((a + b) `mod` p)

instance (Symbolic c, KnownFFA p c, Scale a (Zp p)) => Scale a (FFA p c) where
  scale k x = fromConstant (scale k one :: Zp p) * x

instance (Symbolic c, KnownFFA p c) => Zero (FFA p c) where
  zero = fromConstant (zero :: Zp p)

instance (Symbolic c, KnownFFA p c) => AdditiveMonoid (FFA p c)

instance (Symbolic c, KnownFFA p c) => AdditiveGroup (FFA p c) where
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

instance (Symbolic c, KnownFFA p c) => Semiring (FFA p c)

instance (Symbolic c, KnownFFA p c) => Ring (FFA p c)

instance (Symbolic c, KnownFFA p c, Prime p) => Exponent (FFA p c) Integer where
  x ^ a
    | neg Prelude.< pos = finv x ^ neg
    | otherwise = x ^ pos
   where
    pos = Prelude.fromIntegral (a `mod` Prelude.fromIntegral (value @p -! 1))
    neg = value @p -! (pos + 1)

instance (Symbolic c, KnownFFA p c, Prime p) => Field (FFA p c) where
  finv (FFA nx ux) =
    if isNative @p @c
      then FFA (finv nx) (UIntFFA zero)
      else result
   where
    p :: FromConstant Natural a => a
    p = fromConstant (value @p)
    -- Compute unconstrained Bezout coefficients
    x = valueFFA @p nx ux
    l0 = negate (bezoutL p x)
    r0 = bezoutR p x
    sign = r0 < zero
    FFA nl ul = layoutFFA @p (ifThenElse sign (l0 + x) l0)
    -- Constrain the result
    result =
      assert
        ( \ffa@(FFA nr ur) ->
            isValid (ffa :*: ul) -- result is indeed FFA, UInt is indeed UInt
              && (nr * nx == nl * p + one) -- equation holds modulo basefield
              && (ur * ux == ul * p + one) -- equation holds modulo 2^k
        )
        $ layoutFFA (ifThenElse sign (r0 + p) r0)

instance Finite (Zp p) => Finite (FFA p c) where
  type Order (FFA p c) = p

instance (Symbolic c, KnownFFA p c) => BinaryExpansion (FFA p c) where
  type Bits (FFA p c) = ByteString (NumberOfBits (Zp p)) c
  binaryExpansion = uintToBSbe . toUInt @(NumberOfBits (Zp p))
  fromBinary = unsafeFromUInt @(NumberOfBits (Zp p)) . beBSToUInt

-- | __NOTE__: This function assumes that the given 'UInt' is in the range of the field. Use 'fromUInt' instead if you need to perform a modulo operation (by order of field) on the 'UInt'.
unsafeFromUInt
  :: forall n p c
   . (Symbolic c, KnownUInt n c, KnownFFA p c)
  => UInt n c
  -> FFA p c
unsafeFromUInt ux = FFA (uintToFE ux) (UIntFFA $ resizeUInt ux)

fromUInt
  :: forall n p c
   . (Symbolic c, KnownFFA p c, KnownUInt n c)
  => UInt n c
  -> FFA p c
fromUInt ux =
  let
    uWide = resizeUInt ux
    m :: UInt (FFAMaxBits p c) c = fromConstant (value @p)
   in
    unsafeFromUInt $ mod uWide m

-- | __NOTE__: This function assumes that the given 'Int' is in the range of the field. Use 'fromInt' instead if you need to perform a modulo operation (by order of field) on the 'Int'.
unsafeFromInt :: (Symbolic c, KnownFFA p c, KnownUInt n c) => Int n c -> FFA p c
unsafeFromInt ix =
  let uxFFA = unsafeFromUInt (intToUInt ix)
   in ifThenElse (isNegative ix) (negate uxFFA) uxFFA

fromInt :: (Symbolic c, KnownFFA p c, KnownUInt n c) => Int n c -> FFA p c
fromInt ix =
  let uxFFA = fromUInt (intToUInt ix)
   in ifThenElse (isNegative ix) (negate uxFFA) uxFFA

toUInt
  :: forall n p c
   . (Symbolic c, KnownFFA p c, KnownUInt n c)
  => FFA p c -> UInt n c
toUInt src@(FFA nx ux) =
  assert
    (\res -> isValid res && unsafeFromUInt res == src)
    (uintFromSemiEuclidean (valueFFA @p nx ux))
