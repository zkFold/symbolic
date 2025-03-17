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

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field   (Zp)
import           ZkFold.Base.Algebra.Basic.Number  (KnownNat, Prime, type (*), type (^), value)
import           ZkFold.Base.Data.Vector           (Vector)
import           ZkFold.Symbolic.Class             (Arithmetic, Symbolic (..), fromCircuit2F, symbolicF)
import           ZkFold.Symbolic.Data.Bool         (Bool (..), BoolType (..))
import           ZkFold.Symbolic.Data.ByteString   (ByteString)
import           ZkFold.Symbolic.Data.Class        (SymbolicData (..))
import           ZkFold.Symbolic.Data.Combinators  (Ceil, GetRegisterSize, Iso (..), KnownRegisterSize, KnownRegisters,
                                                    NumberOfRegisters, Resize (..))
import           ZkFold.Symbolic.Data.Conditional  (Conditional (..))
import           ZkFold.Symbolic.Data.Eq           (Eq (..))
import           ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import           ZkFold.Symbolic.Data.Input        (SymbolicInput (..))
import           ZkFold.Symbolic.Data.Ord          (Ord (..))
import           ZkFold.Symbolic.Data.UInt         (OrdWord, UInt (..), natural, register, toNative)
import           ZkFold.Symbolic.Interpreter       (Interpreter (..))
import           ZkFold.Symbolic.MonadCircuit      (MonadCircuit (..), ResidueField (..), Witness (..))

type family FFAUIntSize (p :: Natural) (q :: Natural) :: Natural where
  FFAUIntSize p p = 0
  FFAUIntSize p q = NumberOfBits (Zp (Ceil (p * p) q))

type UIntFFA p r c = UInt (FFAUIntSize p (Order (BaseField c))) r c

data FFA p r c = FFA
  { nativeResidue :: FieldElement c
  , uintResidue   :: UIntFFA p r c
  }
  deriving (Generic)

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
  , KnownNat (NumberOfBits (Zp p)))

instance (Symbolic c, KnownFFA p r c) => SymbolicData (FFA p r c)
instance (Symbolic c, KnownFFA p r c) => SymbolicInput (FFA p r c) where
  isValid ffa@(FFA _ ux) =
    isValid ux && toUInt @(FFAMaxBits p c) ffa < fromConstant (value @p)
instance (NFData (FieldElement c), NFData (UIntFFA p r c)) => NFData (FFA p r c)
instance (Symbolic c, KnownFFA p r c, b ~ Bool c) => Conditional b (FFA p r c)
instance (Symbolic c, KnownFFA p r c) => Eq (FFA p r c)
deriving stock instance (Show (FieldElement c), Show (UIntFFA p r c)) =>
  Show (FFA p r c)

bezoutFFA ::
  forall p a. (KnownNat p, KnownNat (FFAUIntSize p (Order a))) => Integer
bezoutFFA =
  bezoutR (1 `shiftL` Prelude.fromIntegral (value @(FFAUIntSize p (Order a))))
          (fromConstant $ value @p)

instance (Arithmetic a, KnownFFA p r (Interpreter a)) =>
    ToConstant (FFA p r (Interpreter a)) where
  type Const (FFA p r (Interpreter a)) = Zp p
  toConstant (FFA nx ux) =
    let n = fromConstant (toConstant (toConstant nx))
        u = fromConstant (toConstant ux) :: Integer
        -- x = k|a| + n = l*2^s + u
        -- k|a| - l*2^s = u - n
        k = (u - n) * bezoutFFA @p @a
     in fromConstant (k * fromConstant (order @a) + n)

instance (Symbolic c, KnownFFA p r c, FromConstant a (Zp p)) =>
    FromConstant a (FFA p r c) where
  fromConstant c =
    let c' = toConstant (fromConstant c :: Zp p)
     in FFA (fromConstant c') (fromConstant c')

instance {-# OVERLAPPING #-} FromConstant (FFA p r c) (FFA p r c)

instance {-# OVERLAPPING #-}
  (Symbolic c, KnownFFA p r c) => Scale (FFA p r c) (FFA p r c)

valueFFA ::
  forall p r c i a.
  (Symbolic c, KnownFFA p r c, Witness i (WitnessField c), a ~ BaseField c) =>
  (Par1 :*: Vector (NumberOfRegisters a (FFAUIntSize p (Order a)) r)) i ->
  IntegralOf (WitnessField c)
valueFFA (Par1 ni :*: ui) =
  let n = toIntegral (at ni :: WitnessField c)
      u = natural @c @(FFAUIntSize p (Order a)) @r ui
      k = (u - n) * fromConstant (bezoutFFA @p @a)
   in k * fromConstant (order @a) + n

layoutFFA ::
  forall p r c a w.
  (Symbolic c, KnownFFA p r c, a ~ BaseField c, w ~ WitnessField c) =>
  IntegralOf w ->
  (Par1 :*: Vector (NumberOfRegisters a (FFAUIntSize p (Order a)) r)) w
layoutFFA c =
  Par1 (fromIntegral c)
  :*: tabulate (register @c @(FFAUIntSize p (Order a)) @r c)

fromFFA ::
  forall p r a.
  (Arithmetic a, KnownFFA p r (Interpreter a)) =>
  (Par1 :*: Vector (NumberOfRegisters a (FFAUIntSize p (Order a)) r)) a ->
  Integer
fromFFA (Par1 x :*: v) =
  fromConstant $ toConstant $ toConstant
    $ FFA @p @r (fromConstant x) (UInt (Interpreter v))

toFFA ::
  forall p r a.
  (Arithmetic a, KnownFFA p r (Interpreter a)) =>
  Integer ->
  (Par1 :*: Vector (NumberOfRegisters a (FFAUIntSize p (Order a)) r)) a
toFFA n =
  let FFA (FieldElement (Interpreter x)) (UInt (Interpreter v)) =
        fromConstant n :: FFA p r (Interpreter a)
   in x :*: v

instance (Symbolic c, KnownFFA p r c) => MultiplicativeSemigroup (FFA p r c) where
  FFA nx ux * FFA ny uy = FFA nr ur
    where
      p :: FromConstant Natural a => a
      p = fromConstant (value @p)
      -- | Computes unconstrained \(d = ab div p\) and \(m = ab mod p\)
      nd, nm :: FieldElement c
      ud, um :: UIntFFA p r c
      (nd, ud, nm, um) = restore $ const
        ( symbolicF (arithmetize (nx, ux, ny, uy) Proxy)
            (\((fromFFA @p @r -> a) :*: (fromFFA @p @r -> b)) ->
              toFFA @p @r ((a * b) `div` p) :*: toFFA @p @r ((a * b) `mod` p)
            )
            \((valueFFA @p @r @c -> a) :*: (valueFFA @p @r @c -> b)) -> do
              traverse unconstrained
                  $ layoutFFA @p @r @c ((a * b) `div` p)
                  :*: layoutFFA @p @r @c ((a * b) `mod` p)
        , (U1 :*: U1) :*: (U1 :*: U1))
      -- | Constraints:
      -- * UInt registers are indeed registers;
      -- * m < p;
      -- * equation holds modulo basefield;
      -- * equation holds modulo 2^k.
      Bool ck = isValid (ud, FFA @p nm um)
                && (nx * ny == nd * p + nm)
                && (ux * uy == ud * p + um)
      -- | Sew constraints into result.
      (nr, ur) = restore $ const
        ( fromCircuitF (arithmetize (nm, um, ck) Proxy)
            \(ni :*: ui :*: Par1 b) -> do
              constraint (($ b) - one)
              return (ni :*: ui)
        , U1 :*: U1)

instance (Symbolic c, KnownFFA p r c) => Exponent (FFA p r c) Natural where
  x ^ a = x `natPow` (a `mod` (value @p -! 1))

instance (Symbolic c, KnownFFA p r c) => MultiplicativeMonoid (FFA p r c) where
  one = fromConstant (one :: Zp p)

instance (Symbolic c, KnownFFA p r c) => AdditiveSemigroup (FFA p r c) where
  FFA nx ux + FFA ny uy = FFA nr ur
    where
      p :: FromConstant Natural a => a
      p = fromConstant (value @p)
      -- | Computes unconstrained \(d = ab div p\) and \(m = ab mod p\).
      -- \(d\) must be {0, 1} as addition can only overflow so much.
      d :: Bool c
      nm :: FieldElement c
      um :: UIntFFA p r c
      (d, nm, um) = restore $ const
        ( symbolicF (arithmetize (nx, ux, ny, uy) Proxy)
            (\((fromFFA @p @r -> a) :*: (fromFFA @p @r -> b)) ->
              Par1 (if a + b Prelude.>= p then one else zero)
                :*: toFFA @p @r ((a + b) `mod` p)
            )
            \((valueFFA @p @r @c -> a) :*: (valueFFA @p @r @c -> b)) -> do
              traverse unconstrained
                  $ Par1 (fromIntegral ((a + b) `div` p))
                  :*: layoutFFA @p @r @c ((a + b) `mod` p)
        , U1 :*: (U1 :*: U1))
      -- | Constraints:
      -- * boolean is indeed boolean;
      -- * UInt registers are indeed registers;
      -- * m < p;
      -- * equation holds modulo basefield;
      -- * equation holds modulo 2^k.
      Bool ck = isValid (d, FFA @p nm um)
                && (nx + ny == bool zero p d + nm)
                && (ux + uy == bool zero p d + um)
      -- | Sew constraints into result.
      (nr, ur) = restore $ const
        ( fromCircuitF (arithmetize (nm, um, ck) Proxy)
            \(ni :*: ui :*: Par1 b) -> do
              constraint (($ b) - one)
              return (ni :*: ui)
        , U1 :*: U1)

instance (Symbolic c, KnownFFA p r c, Scale a (Zp p)) => Scale a (FFA p r c) where
  scale k x = fromConstant (scale k one :: Zp p) * x

instance (Symbolic c, KnownFFA p r c) => AdditiveMonoid (FFA p r c) where
  zero = fromConstant (zero :: Zp p)

instance (Symbolic c, KnownFFA p r c) => AdditiveGroup (FFA p r c) where
  -- | negate cannot overflow if value is nonzero.
  negate (FFA nx ux) = bool
    (FFA (fromConstant (value @p) - nx) (fromConstant (value @p) - ux))
    (FFA nx ux)
    (nx == zero && ux == zero)

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
  finv (FFA nx ux) = FFA ny uy
    where
      p :: FromConstant Natural a => a
      p = fromConstant (value @p)
      -- | Computes unconstrained Bezout coefficients.
      nl, nr :: FieldElement c
      ul, ur :: UIntFFA p r c
      (nl, ul, nr, ur) = restore $ const
        ( symbolicF (arithmetize (nx, ux) Proxy)
            (\(fromFFA @p @r -> x) ->
              let l0 = negate (bezoutL p x)
                  r0 = bezoutR p x
                  (l, r) = if r0 Prelude.< 0 then (l0 + x, r0 + p) else (l0, r0)
               in toFFA @p @r l :*: toFFA @p @r r
            )
            \(valueFFA @p @r @c -> x) -> do
              let l0 = negate (bezoutL p x)
                  r0 = bezoutR p x
                  s  = r0 `div` p -- -1 when negative, 0 when positive
                  l  = l0 - s * x
                  r  = r0 - s * p
              traverse unconstrained $
                layoutFFA @p @r @c l :*: layoutFFA @p @r @c r
        , (U1 :*: U1) :*: (U1 :*: U1))
      -- | Constraints:
      -- * UInt registers are indeed registers;
      -- * r < p;
      -- * equation holds modulo basefield;
      -- * equation holds modulo 2^k.
      Bool ck = isValid (ur, FFA @p nl ul)
                && (nr * nx == nl * p + one)
                && (ur * ux == ul * p + one)
      -- | Sew constraints into result.
      (ny, uy) = restore $ const
        ( fromCircuitF (arithmetize (nr, ur, ck) Proxy)
            \(ni :*: ui :*: Par1 b) -> do
              constraint (($ b) - one)
              return (ni :*: ui)
        , U1 :*: U1)

instance Finite (Zp p) => Finite (FFA p r c) where
  type Order (FFA p r c) = p

instance (Symbolic c, KnownFFA p r c) => BinaryExpansion (FFA p r c) where
  type Bits (FFA p r c) = ByteString (NumberOfBits (Zp p)) c
  binaryExpansion = from . toUInt @(NumberOfBits (Zp p))
  fromBinary = fromUInt @(NumberOfBits (Zp p)) . from

fromUInt ::
  forall n p r c.
  (Symbolic c, KnownFFA p r c) =>
  (KnownNat n, KnownNat (GetRegisterSize (BaseField c) n r)) =>
  UInt n r c -> FFA p r c
fromUInt ux = FFA (toNative ux) (resize ux)

toUInt ::
  forall n p r c.
  (Symbolic c, KnownFFA p r c) =>
  (KnownNat n, KnownNat (NumberOfRegisters (BaseField c) n r)) =>
  (KnownNat (GetRegisterSize (BaseField c) n r)) =>
  FFA p r c -> UInt n r c
toUInt x = uy
  where
    -- | Computes unconstrained UInt value
    us :: UInt n r c
    us = restore $ const
      ( symbolicF (arithmetize x Proxy)
          (\(fromFFA @p @r -> v) ->
            let UInt (Interpreter f) = fromConstant v
                  :: UInt n r (Interpreter (BaseField c))
             in f
          )
          \(valueFFA @p @r @c -> v) ->
            traverse unconstrained $ tabulate (register @c @n @r v)
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
