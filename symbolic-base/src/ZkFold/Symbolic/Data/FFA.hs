{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.FFA (FFA (..), KnownFFA, FFAMaxBits, toUInt) where

import           Control.DeepSeq                   (NFData)
import           Control.Monad                     (Monad (..))
import           Data.Bits                         (shiftL)
import           Data.Bool                         (otherwise)
import           Data.Function                     (const, ($), (.))
import           Data.Functor                      (($>))
import           Data.Functor.Rep                  (Representable (..))
import           Data.Proxy                        (Proxy (..))
import           Data.Traversable                  (Traversable (..))
import           Data.Type.Equality                (type (~))
import           GHC.Generics                      (Generic, Par1 (..), U1 (..), type (:*:) (..))
import           Numeric.Natural                   (Natural)
import           Prelude                           (Integer)
import qualified Prelude
import           Text.Show                         (Show)

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Field              (Zp)
import           ZkFold.Algebra.Number             (KnownNat, Prime, type (*), type (^), value)
import           ZkFold.Data.HFunctor.Classes      (HNFData, HShow)
import           ZkFold.Data.Vector                (Vector)
import           ZkFold.Symbolic.Class             (Arithmetic, Symbolic (..), fromCircuit2F, symbolicF)
import           ZkFold.Symbolic.Data.Bool         (Bool (..), BoolType (..))
import           ZkFold.Symbolic.Data.ByteString   (ByteString)
import           ZkFold.Symbolic.Data.Class        (SymbolicData (..))
import           ZkFold.Symbolic.Data.Combinators  (Ceil, IsValidRegister, Iso (..), Resize (..))
import           ZkFold.Symbolic.Data.Conditional  (Conditional (..))
import           ZkFold.Symbolic.Data.Eq           (Eq (..))
import           ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import           ZkFold.Symbolic.Data.Input        (SymbolicInput (..))
import           ZkFold.Symbolic.Data.Ord          (Ord (..))
import           ZkFold.Symbolic.Data.UInt         (UInt (..), natural, register, toNative)
import           ZkFold.Symbolic.Interpreter       (Interpreter (..))
import           ZkFold.Symbolic.MonadCircuit      (MonadCircuit (..), ResidueField (..), Witness (..))

type family FFAUIntBits (p :: Natural) (q :: Natural) :: Natural where
  FFAUIntBits p p = 0
  FFAUIntBits p q = NumberOfBits (Zp (Ceil (p * p) q))

isNative :: forall r p c. (Symbolic c, KnownFFA r p c) => Prelude.Bool
isNative = value @p == value @(Order (BaseField c))

type FFAUIntRegisters (r :: Natural) (p :: Natural) (q :: Natural) =
  Ceil (FFAUIntBits p q) r

type UIntFFA r p c = UInt r (FFAUIntRegisters r p (Order (BaseField c))) c

data FFA r p c = FFA
  { nativeResidue :: FieldElement c
  , uintResidue   :: UIntFFA r p c
  }
  deriving (Generic)

type FFAMaxValue p q = q * (2 ^ FFAUIntBits p q)

type FFAMaxBits p c = NumberOfBits (Zp (FFAMaxValue p (Order (BaseField c))))

type FFAMaxRegisters r p c =
  Ceil (FFAMaxBits p c) r

type KnownFFA r p c =
  ( KnownNat r
  , KnownNat p
  , KnownNat (NumberOfBits (Zp p))
  , KnownNat (FFAUIntBits p (Order (BaseField c)))
  , KnownNat (FFAUIntRegisters r p (Order (BaseField c)))
  , KnownNat (FFAMaxRegisters r p c)
  , IsValidRegister 1 (NumberOfBits (Zp p)) c
  , IsValidRegister r (FFAUIntRegisters r p (Order (BaseField c))) c
  , IsValidRegister r (FFAMaxRegisters r p c) c  
  )

instance (KnownFFA r p c, Symbolic c) => SymbolicData (FFA r p c)
instance (KnownFFA r p c, Symbolic c)
  => SymbolicInput (FFA r p c) where
    isValid ffa@(FFA _ ux) =
      if isNative @r @p @c
        then true
        else isValid ux && toUInt @r @(FFAMaxRegisters r p c) ffa < fromConstant (value @p)

instance HNFData c => NFData (FFA r p c)
deriving stock instance HShow c => Show (FFA r p c)
instance (Symbolic c, KnownFFA r p c, b ~ Bool c) => Conditional b (FFA r p c)
instance (Symbolic c, KnownFFA r p c) => Eq (FFA r p c)

bezoutFFA ::
  forall p a. (KnownNat p, KnownNat (FFAUIntBits p (Order a))) => Integer
bezoutFFA =
  bezoutR (1 `shiftL` Prelude.fromIntegral (value @(FFAUIntBits p (Order a))))
          (fromConstant $ value @p)

instance (KnownFFA r p (Interpreter a), Arithmetic a) => ToConstant (FFA r p (Interpreter a)) where
  type Const (FFA r p (Interpreter a)) = Zp p
  toConstant (FFA nx ux) =
    let n = fromConstant (toConstant (toConstant nx))
        u = fromConstant (toConstant ux) :: Integer
        -- x = k|a| + n = l*2^s + u
        -- k|a| - l*2^s = u - n
        k = (u - n) * bezoutFFA @p @a
     in fromConstant (k * fromConstant (order @a) + n)

instance (KnownFFA r p c, Symbolic c, FromConstant a (Zp p)) =>
    FromConstant a (FFA r p c) where
  fromConstant c =
    let c' = toConstant (fromConstant c :: Zp p)
     in FFA (fromConstant c') (fromConstant c')

instance {-# OVERLAPPING #-} FromConstant (FFA r p c) (FFA r p c)

instance {-# OVERLAPPING #-}
  (Symbolic c, KnownFFA r p c) => Scale (FFA r p c) (FFA r p c)

valueFFA ::
  forall r p c n i a .
  ( KnownFFA r p c
  , Symbolic c
  , a ~ BaseField c
  , Witness i (WitnessField c)
  , n ~ FFAUIntRegisters r p (Order (BaseField c))
  ) => (Par1 :*: Vector n) i -> IntegralOf (WitnessField c)
valueFFA (Par1 ni :*: ui) =
  let n = toIntegral (at ni :: WitnessField c)
      u = natural @r @n @c ui
      k = (u - n) * fromConstant (bezoutFFA @p @a)
   in k * fromConstant (order @a) + n

layoutFFA ::
  forall r p c n w .
  ( KnownFFA r p c
  , Symbolic c
  , w ~ WitnessField c
  , n ~ FFAUIntRegisters r p (Order (BaseField c))
  ) => IntegralOf w -> (Par1 :*: Vector n) w
layoutFFA c =
  Par1 (fromIntegral c)
  :*: tabulate (register @r @n @c c)

fromFFA ::
  forall r p a n .
  ( KnownFFA r p (Interpreter a)
  , Arithmetic a
  , n ~ FFAUIntRegisters r p (Order a)
  ) => (Par1 :*: Vector n) a ->
  Integer
fromFFA (Par1 x :*: v) =
  fromConstant $ toConstant $ toConstant
    $ FFA @r @p (fromConstant x) (UInt (Interpreter v))

toFFA ::
  forall r p a n .
  ( KnownFFA r p (Interpreter a)
  , Arithmetic a
  , n ~ FFAUIntRegisters r p (Order a)
  ) => Integer -> (Par1 :*: Vector n) a
toFFA n =
  let FFA (FieldElement (Interpreter x)) (UInt (Interpreter v)) =
        fromConstant n :: FFA r p (Interpreter a)
   in x :*: v

instance (Symbolic c, KnownFFA r p c) => MultiplicativeSemigroup (FFA r p c) where
  FFA nx ux * FFA ny uy =
      if isNative @r @p @c
        then FFA (nx * ny) zero
        else FFA nr ur
    where
      p :: FromConstant Natural a => a
      p = fromConstant (value @p)
      -- | Computes unconstrained \(d = ab div p\) and \(m = ab mod p\)
      nd, nm :: FieldElement c
      ud, um :: UIntFFA r p c
      (nd, ud, nm, um) = restore $ const
        ( symbolicF (arithmetize (nx, ux, ny, uy) Proxy)
            (\((fromFFA @r @p -> a) :*: (fromFFA @r @p -> b)) ->
              toFFA @r @p ((a * b) `div` p) :*: toFFA @r @p ((a * b) `mod` p)
            )
            \((valueFFA @r @p @c -> a) :*: (valueFFA @r @p @c -> b)) -> do
              traverse unconstrained
                  $ layoutFFA @r @p @c ((a * b) `div` p)
                  :*: layoutFFA @r @p @c ((a * b) `mod` p)
        , (U1 :*: U1) :*: (U1 :*: U1))
      -- | Constraints:
      -- * UInt registers are indeed registers;
      -- * m < p;
      -- * equation holds modulo basefield;
      -- * equation holds modulo 2^k.
      Bool ck = isValid (ud, FFA @r @p nm um)
                && (nx * ny == nd * p + nm)
                && (ux * uy == ud * p + um)
      -- | Sew constraints into result.
      (nr, ur) = restore $ const
        ( fromCircuitF (arithmetize (nm, um, ck) Proxy)
            \(ni :*: ui :*: Par1 b) -> do
              constraint (($ b) - one)
              return (ni :*: ui)
        , U1 :*: U1)

instance (Symbolic c, KnownFFA r p c) => Exponent (FFA r p c) Natural where
  x ^ a = x `natPow` (a `mod` (value @p -! 1))

instance (Symbolic c, KnownFFA r p c) => MultiplicativeMonoid (FFA r p c) where
  one = fromConstant (one :: Zp p)

instance (Symbolic c, KnownFFA r p c) => AdditiveSemigroup (FFA r p c) where
  FFA nx ux + FFA ny uy =
      if isNative @r @p @c
        then FFA (nx + ny) zero
        else FFA nr ur
    where
      p :: FromConstant Natural a => a
      p = fromConstant (value @p)
      -- | Computes unconstrained \(d = ab div p\) and \(m = ab mod p\).
      -- \(d\) must be {0, 1} as addition can only overflow so much.
      d :: Bool c
      nm :: FieldElement c
      um :: UIntFFA r p c
      (d, nm, um) = restore $ const
        ( symbolicF (arithmetize (nx, ux, ny, uy) Proxy)
            (\((fromFFA @r @p -> a) :*: (fromFFA @r @p -> b)) ->
              Par1 (if a + b Prelude.>= p then one else zero)
                :*: toFFA @r @p ((a + b) `mod` p)
            )
            \((valueFFA @r @p @c -> a) :*: (valueFFA @r @p @c -> b)) -> do
              traverse unconstrained
                  $ Par1 (fromIntegral ((a + b) `div` p))
                  :*: layoutFFA @r @p @c ((a + b) `mod` p)
        , U1 :*: (U1 :*: U1))
      -- | Constraints:
      -- * boolean is indeed boolean;
      -- * UInt registers are indeed registers;
      -- * m < p;
      -- * equation holds modulo basefield;
      -- * equation holds modulo 2^k.
      Bool ck = isValid (d, FFA @r @p nm um)
                && (nx + ny == bool zero p d + nm)
                && (ux + uy == bool zero p d + um)
      -- | Sew constraints into result.
      (nr, ur) = restore $ const
        ( fromCircuitF (arithmetize (nm, um, ck) Proxy)
            \(ni :*: ui :*: Par1 b) -> do
              constraint (($ b) - one)
              return (ni :*: ui)
        , U1 :*: U1)

instance (Symbolic c, KnownFFA r p c, Scale a (Zp p)) => Scale a (FFA r p c) where
  scale k x = fromConstant (scale k one :: Zp p) * x

instance (Symbolic c, KnownFFA r p c) => AdditiveMonoid (FFA r p c) where
  zero = fromConstant (zero :: Zp p)

instance (Symbolic c, KnownFFA r p c) => AdditiveGroup (FFA r p c) where
  -- | negate cannot overflow if value is nonzero.
  negate (FFA nx ux) =
    if isNative @r @p @c
      then FFA (negate nx) zero
      else bool
        (FFA (fromConstant (value @p) - nx) (fromConstant (value @p) - ux))
        (FFA nx ux)
        (nx == zero && ux == zero)

instance (Symbolic c, KnownFFA r p c) => Semiring (FFA r p c)

instance (Symbolic c, KnownFFA r p c) => Ring (FFA r p c)

instance (Symbolic c, KnownFFA r p c, Prime p) => Exponent (FFA r p c) Integer where
  x ^ a
    | neg Prelude.< pos = finv x ^ neg
    | otherwise = x ^ pos
    where
      pos = Prelude.fromIntegral (a `mod` Prelude.fromIntegral (value @p -! 1))
      neg = value @p -! (pos + 1)

instance (Symbolic c, KnownFFA r p c, Prime p) => Field (FFA r p c) where
  finv (FFA nx ux) =
      if isNative @r @p @c
        then FFA (finv nx) zero
        else FFA ny uy
    where
      p :: FromConstant Natural a => a
      p = fromConstant (value @p)
      -- | Computes unconstrained Bezout coefficients.
      nl, nr :: FieldElement c
      ul, ur :: UIntFFA r p c
      (nl, ul, nr, ur) = restore $ const
        ( symbolicF (arithmetize (nx, ux) Proxy)
            (\(fromFFA @r @p -> x) ->
              let l0 = negate (bezoutL p x)
                  r0 = bezoutR p x
                  (l, r) = if r0 Prelude.< 0 then (l0 + x, r0 + p) else (l0, r0)
               in toFFA @r @p l :*: toFFA @r @p r
            )
            \(valueFFA @r @p @c -> x) -> do
              let l0 = negate (bezoutL p x)
                  r0 = bezoutR p x
                  s  = r0 `div` p -- -1 when negative, 0 when positive
                  l  = l0 - s * x
                  r  = r0 - s * p
              traverse unconstrained $
                layoutFFA @r @p @c l :*: layoutFFA @r @p @c r
        , (U1 :*: U1) :*: (U1 :*: U1))
      -- | Constraints:
      -- * UInt registers are indeed registers;
      -- * r < p;
      -- * equation holds modulo basefield;
      -- * equation holds modulo 2^k.
      Bool ck = isValid (ur, FFA @r @p nl ul)
                && (nr * nx == nl * p + one)
                && (ur * ux == ul * p + one)
      -- | Sew constraints into result.
      (ny, uy) = restore $ const
        ( fromCircuitF (arithmetize (nr, ur, ck) Proxy)
            \(ni :*: ui :*: Par1 b) -> do
              constraint (($ b) - one)
              return (ni :*: ui)
        , U1 :*: U1)

instance Finite (Zp p) => Finite (FFA r p c) where
  type Order (FFA r p c) = p

instance (Symbolic c, KnownFFA r p c) => BinaryExpansion (FFA r p c) where
  type Bits (FFA r p c) = ByteString (NumberOfBits (Zp p)) c
  binaryExpansion = from . toUInt @1 @(NumberOfBits (Zp p))
  fromBinary = fromUInt @1 @(NumberOfBits (Zp p)) . from

fromUInt ::
  forall r' n r p c .
  ( KnownFFA r p c
  , Symbolic c
  , KnownNat r'
  , KnownNat n
  , IsValidRegister r' n c
  ) => UInt r' n c -> FFA r p c
fromUInt ux = FFA (toNative ux) (resize ux)

toUInt ::
  forall r' n r p c .
  ( KnownFFA r p c
  , Symbolic c
  , KnownNat r'
  , KnownNat n
  , IsValidRegister r' n c
  ) => FFA r p c -> UInt r' n c
toUInt x = uy
  where
    -- | Computes unconstrained UInt value
    us :: UInt r' n c
    us = restore $ const
      ( symbolicF (arithmetize x Proxy)
          (\(fromFFA @r @p -> v) ->
            let UInt (Interpreter f) = fromConstant v
                  :: UInt r' n (Interpreter (BaseField c))
             in f
          )
          \(valueFFA @r @p @c -> v) ->
            traverse unconstrained $ tabulate (register @r @n @c v)
      , U1)
    -- | Constraints:
    -- * UInt registers are indeed registers;
    -- * casting back yields source residues.
    Bool ck = isValid us && fromUInt us == x
    -- | Sew constraints into result.
    uy = restore $ const
      ( fromCircuit2F (arithmetize us Proxy) ck
          \xi (Par1 b) -> constraint (($ b) - one) $> xi
      , U1)
