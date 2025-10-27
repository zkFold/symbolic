{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

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
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Compat (CompatContext (..), CompatData (CompatData, compatData))
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
import ZkFold.Symbolic.Data.Int (Int, isNegative, uint)
import ZkFold.Symbolic.Data.Ord (Ord (..))
import ZkFold.Symbolic.Data.UInt (OrdWord, UInt (..), natural, register, toNative)
import ZkFold.Symbolic.Data.V2 (SymbolicData (..))
import ZkFold.Symbolic.Interpreter (Interpreter (..))
import ZkFold.Symbolic.MonadCircuit (MonadCircuit (..), Witness (..))
import ZkFold.Symbolic.V2 (Symbolic (constrain), (=!=))
import ZkFold.Data.Product (fstP)
import ZkFold.Symbolic.Data.Input (isValid)

type family FFAUIntSize (p :: Natural) (q :: Natural) :: Natural where
  FFAUIntSize p p = 0
  FFAUIntSize p q = NumberOfBits (Zp (Ceil (p * p) q))

isNative :: forall p c. (Symbolic c, KnownNat p) => Prelude.Bool
isNative = value @p == value @(Order c)

newtype UIntFFA p r c = UIntFFA
  {uintFFA :: CompatData (UInt (FFAUIntSize p (Order c)) r) c}
  deriving (Eq, NFData, Prelude.Eq, Show)

instance SymbolicData (UIntFFA p r) where
  type Layout (UIntFFA p r) c = Layout (CompatData (UInt (FFAUIntSize p (Order c)) r)) c
  type HasRep (UIntFFA p r) c = KnownFFA p r c
  toLayout = toLayout . uintFFA
  interpolate b = UIntFFA . interpolate b . fmap (uintFFA <$>)
  fromLayout = UIntFFA . fromLayout

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

-- TODO: Restore SymbolicInput class after refactoring of Bool

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
  :: forall p r c . (Symbolic c, KnownFFA p r c)
  => CompatData FieldElement c -> UIntFFA p r c -> IntegralOf c
valueFFA
  (toLayout -> Par1 ni :*: _)
  (natural @c @c @(FFAUIntSize p (Order c)) @r . fstP . toLayout -> ui) =
  integralFromFFA @p @(Order c) ni ui

layoutFFA
  :: forall p r c . (Symbolic c, KnownFFA p r c)
  => IntegralOf c -> (CompatData FieldElement c, UIntFFA p r c)
layoutFFA c =
  ( CompatData $ FieldElement $ CompatContext $ Par1 (fromConstant c)
  , UIntFFA $ CompatData $ UInt $ CompatContext
  $ tabulate (register @c @(FFAUIntSize p (Order c)) @r @c c)
  )

assert
  :: (SymbolicData f, HasRep f c, Symbolic c)
  => (f c -> CompatData Bool c) -> f c -> f c
assert pred x =
  let CompatData (Bool (CompatContext (Par1 check))) = pred x
   in fromLayout $ constrain (fromConstant check =!= one) <$> toLayout x

instance (Symbolic c, KnownFFA p r c) => MultiplicativeSemigroup (FFA p r c) where
  FFA nx ux * FFA ny uy =
    if isNative @p @c
      then FFA (nx * ny) (UIntFFA zero)
      else result
   where
    p :: FromConstant Natural a => a
    p = fromConstant (value @p)
    -- Compute unconstrained \(d = ab div p\) and \(m = ab mod p\)
    a = valueFFA @p @r nx ux
    b = valueFFA @p @r ny uy
    (di, mi) = (a * b) `divMod` p
    (nd, ud) = layoutFFA @p @r di
    (nm, um) = layoutFFA @p @r mi
    -- Constrain the result
    result = assert (\m ->
        CompatData (isValid $ compatData (uintFFA ud)
                          :*: compatData (uintFFA um)) -- UInt registers are bound;
        && toUInt @(FFAMaxBits p c) m < p -- m < p;
        && (nx * ny == nd * p + nm) -- equation holds modulo basefield and...
        && (uintFFA ux * uintFFA uy == uintFFA ud * p + uintFFA um) -- modulo 2^k.
      ) (FFA @p nm um)

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
    -- \| Computes unconstrained \(d = (a+b) div p\) and \(m = (a+b) mod p\).
    -- \(d\) must be {0, 1} as addition can only overflow so much.
    d :: CompatData Bool c
    nm :: CompatData FieldElement c
    um :: UIntFFA p r c
    d :*: nm :*: um = _
    -- restore
    --   ( symbolicF
    --       (arithmetize ((nx :*: ux) :*: (ny :*: uy)))
    --       ( \((fromFFA @p @r -> a) :*: (fromFFA @p @r -> b)) ->
    --           Par1 (if a + b Prelude.>= p then one else zero)
    --             :*: toFFA @p @r ((a + b) `mod` p)
    --       )
    --       \((valueFFA @p @r @c -> a) :*: (valueFFA @p @r @c -> b)) -> do
    --         traverse unconstrained $
    --           Par1 (fromConstant ((a + b) `div` p))
    --             :*: layoutFFA @p @r @c ((a + b) `mod` p)
    --   , U1 :*: (U1 :*: U1)
    --   )
    -- \| Constraints:
    -- \* boolean is indeed boolean;
    -- \* UInt registers are indeed registers;
    -- \* m < p;
    -- \* equation holds modulo basefield;
    -- \* equation holds modulo 2^k.
    ck =
      -- isValid (d :*: FFA @p nm um)
      (nx + ny == bool zero p d + nm)
        && (uintFFA ux + uintFFA uy == bool zero p d + uintFFA um)
    -- \| Sew constraints into result.
    nr :*: ur = _

-- restore
--   ( fromCircuitF
--       (arithmetize (nm :*: um :*: ck))
--       \(ni :*: ui :*: Par1 b) -> do
--         constraint (($ b) - one)
--         return (ni :*: ui)
--   , U1 :*: U1
--   )

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
    nl, nr :: CompatData FieldElement c
    ul, ur :: UIntFFA p r c
    (nl :*: ul) :*: (nr :*: ur) = _
    -- restore
    --  ( symbolicF
    --      (arithmetize (nx :*: ux))
    --      ( \(fromFFA @p @r -> x) ->
    --          let l0 = negate (bezoutL p x)
    --              r0 = bezoutR p x
    --              (l, r) = if r0 Prelude.< 0 then (l0 + x, r0 + p) else (l0, r0)
    --           in toFFA @p @r l :*: toFFA @p @r r
    --      )
    --      \(valueFFA @p @r @c -> x) -> do
    --        let l0 = negate (bezoutL p x)
    --            r0 = bezoutR p x
    --            s = r0 `div` p -- -1 when negative, 0 when positive
    --            l = l0 - s * x
    --            r = r0 - s * p
    --        traverse unconstrained $
    --          layoutFFA @p @r @c l :*: layoutFFA @p @r @c r
    --  , (U1 :*: U1) :*: (U1 :*: U1)
    --  )
    -- \| Constraints:
    -- \* UInt registers are indeed registers;
    -- \* r < p;
    -- \* equation holds modulo basefield;
    -- \* equation holds modulo 2^k.
    ck = _
    -- isValid (ur :*: FFA @p nl ul)
    --  && (nr * nx == nl * p + one)
    --  && (uintFFA ur * uintFFA ux == uintFFA ul * p + one)
    -- \| Sew constraints into result.
    ny :*: uy = _

-- restore
--  ( fromCircuitF
--      (arithmetize (nr :*: ur :*: ck))
--      \(ni :*: ui :*: Par1 b) -> do
--        constraint (($ b) - one)
--        return (ni :*: ui)
--  , U1 :*: U1
--  )

instance Finite (Zp p) => Finite (FFA p r c) where
  type Order (FFA p r c) = p

instance (Symbolic c, KnownFFA p r c) => BinaryExpansion (FFA p r c) where
  type Bits (FFA p r c) = CompatData (ByteString (NumberOfBits (Zp p))) c
  binaryExpansion = CompatData . from . compatData . toUInt @(NumberOfBits (Zp p))
  fromBinary = unsafeFromUInt @(NumberOfBits (Zp p)) . CompatData . from . compatData

-- | __NOTE__: This function assumes that the given 'UInt' is in the range of the field. Use 'fromUInt' instead if you need to perform a modulo operation (by order of field) on the 'UInt'.
unsafeFromUInt
  :: forall n p r c
   . (Symbolic c, KnownFFA p r c)
  => (KnownNat n, KnownNat (GetRegisterSize c n r))
  => CompatData (UInt n r) c
  -> FFA p r c
unsafeFromUInt (CompatData ux) =
  FFA (CompatData $ toNative ux) (UIntFFA $ CompatData $ resize ux)

fromUInt
  :: forall n p r c
   . (Symbolic c, KnownFFA p r c)
  => KnownNat n
  => CompatData (UInt n r) c
  -> FFA p r c
fromUInt (CompatData ux) =
  let
    uWide = resize ux
    m :: UInt (FFAMaxBits p c) r (CompatContext c) = fromConstant (value @p)
   in
    unsafeFromUInt $ CompatData $ mod uWide m

-- | __NOTE__: This function assumes that the given 'Int' is in the range of the field. Use 'fromInt' instead if you need to perform a modulo operation (by order of field) on the 'Int'.
unsafeFromInt
  :: (Symbolic c, KnownFFA p r c)
  => (KnownNat n, KnownNat (GetRegisterSize c n r))
  => CompatData (Int n r) c
  -> FFA p r c
unsafeFromInt (CompatData ix) =
  let uxFFA = unsafeFromUInt $ CompatData (uint ix)
   in ifThenElse (CompatData $ isNegative ix) (negate uxFFA) uxFFA

fromInt
  :: (Symbolic c, KnownFFA p r c)
  => KnownNat n
  => CompatData (Int n r) c
  -> FFA p r c
fromInt (CompatData ix) =
  let uxFFA = fromUInt $ CompatData (uint ix)
   in ifThenElse (CompatData $ isNegative ix) (negate uxFFA) uxFFA

toUInt
  :: forall n p r c
   . Symbolic c
  => (KnownFFA p r c, KnownNat n, KnownNat (NumberOfRegisters c n r))
  => KnownNat (GetRegisterSize c n r)
  => FFA p r c
  -> CompatData (UInt n r) c
toUInt x = uy
 where
  -- \| Computes unconstrained UInt value
  us :: CompatData (UInt n r) c
  us = _
  -- restore
  --   ( symbolicF
  --       (arithmetize x)
  --       ( \(fromFFA @p @r -> v) ->
  --           let UInt (Interpreter f) =
  --                fromConstant v
  --                  :: UInt n r (Interpreter c)
  --           in f
  --      )
  --      \(valueFFA @p @r @c -> v) ->
  --        traverse unconstrained $ tabulate (register @c @n @r v)
  --  , U1
  --  )
  -- \| Constraints:
  -- \* UInt registers are indeed registers;
  -- \* casting back yields source residues.
  Bool ck = _ -- isValid us && unsafeFromUInt us == x
  -- \| Sew constraints into result.
  uy = _

-- restore
--   ( fromCircuit2F
--       (arithmetize us)
--       ck
--       \xi (Par1 b) -> constraint (($ b) - one) $> xi
--   , U1
--   )
