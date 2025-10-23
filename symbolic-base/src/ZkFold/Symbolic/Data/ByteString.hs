{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.ByteString (
  ByteString (..),
  ShiftBits (..),
  feToBSbe,
  beBSToFE,
  regToBSbe,
  beBSToReg,
  resize,
  reverseEndianness,
  set,
  unset,
  isSet,
  isUnset,
  toWords,
  concat,
  truncate,
  dropN,
  append,
  split,
  emptyByteString,
  toBsBits,
  orRight,
) where

import Control.Applicative (pure)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.ByteString as Bytes
import Data.Constraint (withDict)
import Data.Constraint.Nat (Max, plusMinusInverse3)
import Data.Functor.Rep (index)
import Data.List (reverse, unfoldr)
import Data.Maybe (Maybe (..))
import Data.Ord (Ordering (..), compare)
import Data.String (IsString (..))
import Data.These (These (..))
import qualified Data.Vector as UV
import GHC.Generics (Generic, Generic1)
import GHC.Natural (naturalFromInteger)
import Numeric (readHex, showHex)
import Test.QuickCheck (Arbitrary (arbitrary))
import Text.Show (Show)
import Prelude (
  Integer,
  fmap,
  otherwise,
  return,
  take,
  ($),
  (.),
  (<),
  (<$>),
  (<>),
  (==),
  (>),
  (>=),
  type (~),
 )
import qualified Prelude as Haskell

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Data.Collect (Collect (..))
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.Vector (Vector (..))
import qualified ZkFold.Data.Vector as V
import ZkFold.Prelude (replicate)
import ZkFold.Symbolic.Class (Arithmetic, Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (..), Conditional (..))
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import ZkFold.Symbolic.Data.Register (Register, bitsOfR, fromBinaryR)
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)

-- | A ByteString which stores @n@ bits and uses elements of @a@ as registers, one element per register.
-- Bit layout is Big-endian.
newtype ByteString n c = ByteString (Vector n (Bool c))
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)
  deriving newtype (Collect (ConstrainedDatum c), Eq, Show)

instance (KnownNat n, Symbolic c) => Arbitrary (ByteString n c) where
  arbitrary = ByteString <$> arbitrary

instance (Symbolic c, m * 8 ~ n, KnownNat m) => IsString (ByteString n c) where
  fromString = fromConstant . fromString @Bytes.ByteString

instance
  (Symbolic c, m * 8 ~ n, KnownNat m)
  => FromConstant Bytes.ByteString (ByteString n c)
  where
  fromConstant bytes =
    concat @_ @8 $
      fromConstant @Natural @(ByteString 8 c)
        . Haskell.fromIntegral
        . Haskell.toInteger
        <$> V.unsafeToVector @m paddedBytes
   where
    desiredLen = value @m
    actualLen = Haskell.fromIntegral $ Bytes.length bytes
    paddedBytes = replicate (desiredLen -! actualLen) 0 <> Bytes.unpack bytes

emptyByteString :: FromConstant Natural (ByteString 0 c) => ByteString 0 c
emptyByteString = fromConstant @Natural 0

-- | A class for data types that support bit shift and bit cyclic shift (rotation) operations.
class ShiftBits a where
  {-# MINIMAL (shiftBits | (shiftBitsL, shiftBitsR)), (rotateBits | (rotateBitsL, rotateBitsR)) #-}

  -- | shiftBits performs a left shift when its agrument is greater than zero and a right shift otherwise.
  shiftBits :: a -> Integer -> a
  shiftBits a s
    | s < 0 = shiftBitsR a (Haskell.fromIntegral . negate $ s)
    | otherwise = shiftBitsL a (Haskell.fromIntegral s)

  shiftBitsL :: a -> Natural -> a
  shiftBitsL a s = shiftBits a (Haskell.fromIntegral s)

  shiftBitsR :: a -> Natural -> a
  shiftBitsR a s = shiftBits a (negate . Haskell.fromIntegral $ s)

  -- | rotateBits performs a left cyclic shift when its agrument is greater than zero and a right cyclic shift otherwise.
  rotateBits :: a -> Integer -> a
  rotateBits a s
    | s < 0 = rotateBitsR a (Haskell.fromIntegral . negate $ s)
    | otherwise = rotateBitsL a (Haskell.fromIntegral s)

  rotateBitsL :: a -> Natural -> a
  rotateBitsL a s = rotateBits a (Haskell.fromIntegral s)

  rotateBitsR :: a -> Natural -> a
  rotateBitsR a s = rotateBits a (negate . Haskell.fromIntegral $ s)

instance Arithmetic a => ToConstant (ByteString n a) where
  type Const (ByteString n a) = Natural
  toConstant (ByteString bits) =
    Haskell.foldl (\y p -> toConstant (fromBool p) + base * y) 0 bits
   where
    base = 2

-- | Pack a ByteString using one field element per bit.
-- @fromConstant@ discards bits after @n@.
-- If the constant is greater than @2^n@, only the part modulo @2^n@ will be converted into a ByteString.
instance (Symbolic c, KnownNat n) => FromConstant Natural (ByteString n c) where
  fromConstant n = ByteString $ V.unsafeToVector $ fromConstant <$> toBsBits n (value @n)

instance (Symbolic c, KnownNat n) => FromConstant Integer (ByteString n c) where
  fromConstant = fromConstant . naturalFromInteger . (`Haskell.mod` (2 ^ value @n))

reverseEndianness'
  :: forall wordSize k m x
   . ( KnownNat wordSize
     , m * 8 ~ wordSize
     )
  => Vector (k * wordSize) x -> Vector (k * wordSize) x
reverseEndianness' v =
  let chunks = V.chunks @k @wordSize v
      chunks' = fmap (V.concat . V.reverse . V.chunks @m @8) chunks
   in V.concat chunks'

reverseEndianness
  :: forall wordSize k c m {n}
   . ( KnownNat wordSize
     , n ~ k * wordSize
     , m * 8 ~ wordSize
     )
  => ByteString n c -> ByteString n c
reverseEndianness (ByteString v) = ByteString $ reverseEndianness' @wordSize @k v

instance (Symbolic c, KnownNat n) => Conditional (ByteString n c) (ByteString n c) where
  bool onFalse onTrue condition =
    (condition && onTrue) || (not condition && onFalse)

instance (Symbolic c, KnownNat n) => BoolType (ByteString n c) where
  false = ByteString (pure false)
  true = not false
  not (ByteString bits) = ByteString (not <$> bits)
  ByteString l || ByteString r = ByteString (V.zipWith (||) l r)
  ByteString l && ByteString r = ByteString (V.zipWith (&&) l r)
  ByteString l `xor` ByteString r = ByteString (V.zipWith xor l r)

orRight
  :: forall m n c
   . Symbolic c
  => ByteString m c -> ByteString n c -> ByteString (Max m n) c
orRight (ByteString l) (ByteString r) = ByteString (op <$> V.alignRight l r)
 where
  op = \case
    These x y -> x || y
    This x -> x
    That x -> x

-- | A ByteString of length @n@ can only be split into words of length @wordSize@ if all of the following conditions are met:
-- 1. @wordSize@ is not greater than @n@;
-- 2. @wordSize@ is not zero;
-- 3. The bytestring is not empty;
-- 4. @wordSize@ divides @n@.
toWords
  :: forall m wordSize c
   . KnownNat wordSize
  => ByteString (m * wordSize) c -> Vector m (ByteString wordSize c)
toWords (ByteString bits) = ByteString <$> V.chunks @m @wordSize bits

concat :: forall k m c. Vector k (ByteString m c) -> ByteString (k * m) c
concat = ByteString . V.concat . fmap (\(ByteString bits) -> bits)

truncate :: forall m n c. KnownNat n => ByteString m c -> ByteString n c
truncate (ByteString bits) = ByteString (V.take bits)

dropN :: forall n m c. (KnownNat (m - n), n <= m) => ByteString m c -> ByteString n c
dropN (ByteString bits) =
  withDict (plusMinusInverse3 @n @m) $ ByteString $ V.drop @(m - n) bits

append
  :: forall m n c
   . ByteString m c
  -> ByteString n c
  -> ByteString (m + n) c
append (ByteString b1) (ByteString b2) = ByteString (b1 `V.append` b2)

split :: KnownNat m => ByteString (m + n) c -> (ByteString m c, ByteString n c)
split (ByteString b) =
  let (b1, b2) = V.splitAt b in (ByteString b1, ByteString b2)

regToBSbe :: (KnownNat n, Symbolic c) => Register n c -> ByteString n c
regToBSbe = ByteString . V.reverse . bitsOfR

beBSToReg :: Symbolic c => ByteString n c -> Register n c
beBSToReg (ByteString b) = fromBinaryR (V.reverse b)

--------------------------------------------------------------------------------

instance (Symbolic c, KnownNat n) => ShiftBits (ByteString n c) where
  rotateBits (ByteString bits) s = ByteString $ bits `V.rotate` s
  shiftBits bs@(ByteString (V.toV -> oldBits)) s
    | Haskell.abs s >= integral @n = false
    | otherwise = case compare s 0 of
        LT -> fromVector $ UV.take (integral @n) (zeros <> oldBits)
        EQ -> bs
        GT -> fromVector $ UV.drop (Haskell.fromIntegral s) (oldBits <> zeros)
   where
    zeros = UV.replicate (Haskell.fromIntegral $ Haskell.abs s) false
    fromVector = ByteString . V.Vector

resize
  :: forall c k n
   . (Symbolic c, KnownNat k, KnownNat n)
  => ByteString k c -> ByteString n c
resize (ByteString (V.toV -> v))
  | diff > 0 = ByteString $ V.Vector (UV.replicate diff false <> v)
  | otherwise = ByteString . V.Vector $ UV.drop (Haskell.abs diff) v
 where
  diff :: Haskell.Int
  diff = integral @n Haskell.- integral @k

set
  :: forall c n
   . (Symbolic c, KnownNat n)
  => ByteString n c -> Natural -> ByteString n c
set (ByteString bits) ix = ByteString $ bits V.// [(fromConstant ix, true)]

unset
  :: forall c n
   . (Symbolic c, KnownNat n)
  => ByteString n c -> Natural -> ByteString n c
unset (ByteString bits) ix = ByteString $ bits V.// [(fromConstant ix, false)]

isSet :: forall c n. KnownNat n => ByteString n c -> Natural -> Bool c
isSet (ByteString bits) = index bits . fromConstant

isUnset
  :: forall c n. (Symbolic c, KnownNat n) => ByteString n c -> Natural -> Bool c
isUnset bits = not . isSet bits

--------------------------------------------------------------------------------

toBsBits :: Natural -> Natural -> [Haskell.Bool]
toBsBits num n = reverse bits
 where
  base = 2

  availableBits = unfoldr (toBase base) (num `Haskell.mod` (2 Haskell.^ n)) <> Haskell.repeat 0

  bits = (== 1) <$> take (Haskell.fromIntegral n) availableBits

-- | Convert a number into @base@-ary system.
toBase :: Natural -> Natural -> Maybe (Natural, Natural)
toBase _ 0 = Nothing
toBase base b = let (d, m) = b `divMod` base in Just (m, d)

feToBSbe :: Symbolic c => FieldElement c -> ByteString (NumberOfBits c) c
feToBSbe =
  ByteString . fmap (Bool . fromFieldElement) . V.reverse . binaryExpansion

beBSToFE :: Symbolic c => ByteString (NumberOfBits c) c -> FieldElement c
beBSToFE (ByteString a) =
  fromBinary $ V.reverse $ fmap (FieldElement . fromBool) a

instance
  (Symbolic c, KnownNat n)
  => FromJSON (ByteString n c)
  where
  parseJSON val = do
    str <- parseJSON val
    case hexToByteString @c @n str of
      Nothing -> Haskell.fail "bad bytestring!"
      Just a -> return a

instance Arithmetic a => ToJSON (ByteString n a) where
  toJSON = toJSON . byteStringToHex

byteStringToHex :: Arithmetic a => ByteString n a -> Haskell.String
byteStringToHex bytes = showHex (toConstant bytes) ""

hexToByteString :: (Symbolic c, KnownNat n) => Haskell.String -> Maybe (ByteString n c)
hexToByteString str = case readHex str of
  [(n, "")] -> Just (fromConstant @Natural n)
  _ -> Nothing
