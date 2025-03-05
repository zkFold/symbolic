{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -freduction-depth=0 #-} -- Avoid reduction overflow error caused by NumberOfRegisters
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module ZkFold.Symbolic.Data.ByteString
    ( ByteString(..)
    , ShiftBits (..)
    , Resize (..)
    , reverseEndianness
    , set
    , unset
    , isSet
    , isUnset
    , toWords
    , concat
    , truncate
    , dropN
    , append
    , emptyByteString
    , toBsBits
    , orRight
    ) where

import           Control.DeepSeq                   (NFData)
import           Control.Monad                     (forM, replicateM)
import           Data.Aeson                        (FromJSON (..), ToJSON (..))
import qualified Data.Bits                         as B
import qualified Data.ByteString                   as Bytes
import           Data.Constraint                   (withDict)
import           Data.Constraint.Nat               (Max, plusMinusInverse3)
import           Data.Foldable                     (foldlM)
import           Data.Kind                         (Type)
import           Data.List                         (reverse, unfoldr)
import           Data.Maybe                        (Maybe (..))
import           Data.String                       (IsString (..))
import           Data.These                        (These (..))
import           Data.Traversable                  (for, mapM)
import           GHC.Generics                      (Generic, Par1 (..))
import           GHC.Natural                       (naturalFromInteger)
import           Numeric                           (readHex, showHex)
import           Prelude                           (Integer, const, drop, fmap, otherwise, pure, return, take, type (~),
                                                    ($), (.), (<$>), (<), (<>), (==), (>), (>=))
import qualified Prelude                           as Haskell
import           Test.QuickCheck                   (Arbitrary (..), chooseInteger)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Data.HFunctor         (HFunctor (..))
import           ZkFold.Base.Data.Package          (packWith, unpackWith)
import           ZkFold.Base.Data.Utils            (zipWithM)
import qualified ZkFold.Base.Data.Vector           as V
import           ZkFold.Base.Data.Vector           (Vector (..))
import           ZkFold.Prelude                    (replicate, replicateA, (!!))
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool         (Bool (..), BoolType (..))
import           ZkFold.Symbolic.Data.Class        (SymbolicData)
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Conditional  (Conditional)
import           ZkFold.Symbolic.Data.Eq           (Eq)
import           ZkFold.Symbolic.Data.FieldElement (FieldElement)
import           ZkFold.Symbolic.Data.Input        (SymbolicInput, isValid)
import           ZkFold.Symbolic.Interpreter       (Interpreter (..))
import           ZkFold.Symbolic.MonadCircuit      (ClosedPoly, newAssigned)

-- | A ByteString which stores @n@ bits and uses elements of @a@ as registers, one element per register.
-- Bit layout is Big-endian.
--
newtype ByteString (n :: Natural) (context :: (Type -> Type) -> Type) = ByteString (context (Vector n))
    deriving (Generic)

deriving stock instance Haskell.Show (c (Vector n)) => Haskell.Show (ByteString n c)
deriving stock instance Haskell.Eq (c (Vector n)) => Haskell.Eq (ByteString n c)
deriving anyclass instance NFData (c (Vector n)) => NFData (ByteString n c)
deriving newtype instance (KnownNat n, Symbolic c) => SymbolicData (ByteString n c)
deriving newtype instance (Symbolic c, KnownNat n) => Eq (ByteString n c)
deriving newtype instance (Symbolic c, KnownNat n) => Conditional (Bool c) (ByteString n c)

instance
    ( Symbolic c
    , m * 8 ~ n
    , KnownNat m
    ) => IsString (ByteString n c) where
    fromString = fromConstant . fromString @Bytes.ByteString

instance
    ( Symbolic c
    , m * 8 ~ n
    , KnownNat m
    ) => FromConstant Bytes.ByteString (ByteString n c) where
    fromConstant bytes = concat @_ @8 $ fromConstant @Natural @(ByteString 8 c)
        . Haskell.fromIntegral
        . Haskell.toInteger <$> (V.unsafeToVector @m paddedBytes)

        where
            desiredLen = value @m
            actualLen = Haskell.fromIntegral $ Bytes.length bytes
            paddedBytes = replicate (desiredLen -! actualLen) 0 <> Bytes.unpack bytes


emptyByteString :: FromConstant Natural (ByteString 0 c) => ByteString 0 c
emptyByteString = fromConstant @Natural 0

-- | A class for data types that support bit shift and bit cyclic shift (rotation) operations.
--
class ShiftBits a where
    {-# MINIMAL (shiftBits | (shiftBitsL, shiftBitsR)), (rotateBits | (rotateBitsL, rotateBitsR)) #-}

    -- | shiftBits performs a left shift when its agrument is greater than zero and a right shift otherwise.
    --
    shiftBits :: a -> Integer -> a
    shiftBits a s
      | s < 0     = shiftBitsR a (Haskell.fromIntegral . negate $ s)
      | otherwise = shiftBitsL a (Haskell.fromIntegral s)

    shiftBitsL :: a -> Natural -> a
    shiftBitsL a s = shiftBits a (Haskell.fromIntegral s)

    shiftBitsR :: a -> Natural -> a
    shiftBitsR a s = shiftBits a (negate . Haskell.fromIntegral $ s)

    -- | rotateBits performs a left cyclic shift when its agrument is greater than zero and a right cyclic shift otherwise.
    --
    rotateBits :: a -> Integer -> a
    rotateBits a s
      | s < 0     = rotateBitsR a (Haskell.fromIntegral . negate $ s)
      | otherwise = rotateBitsL a (Haskell.fromIntegral s)

    rotateBitsL :: a -> Natural -> a
    rotateBitsL a s = rotateBits a (Haskell.fromIntegral s)

    rotateBitsR :: a -> Natural -> a
    rotateBitsR a s = rotateBits a (negate . Haskell.fromIntegral $ s)



instance Arithmetic a => ToConstant (ByteString n (Interpreter a)) where
    type Const (ByteString n (Interpreter a)) = Natural
    toConstant (ByteString (Interpreter bits)) = Haskell.foldl (\y p -> toConstant p + base * y) 0 bits
        where base = 2


-- | Pack a ByteString using one field element per bit.
-- @fromConstant@ discards bits after @n@.
-- If the constant is greater than @2^n@, only the part modulo @2^n@ will be converted into a ByteString.
instance (Symbolic c, KnownNat n) => FromConstant Natural (ByteString n c) where
    fromConstant n = ByteString . embed @c $ V.unsafeToVector $ fromConstant <$> toBsBits n (value @n)

instance (Symbolic c, KnownNat n) => FromConstant Integer (ByteString n c) where
    fromConstant = fromConstant . naturalFromInteger . (`Haskell.mod` (2 ^ getNatural @n))

instance (Symbolic c, KnownNat n) => Arbitrary (ByteString n c) where
    arbitrary = ByteString . embed @c . V.unsafeToVector <$> replicateA (value @n) (toss (1 :: Natural))
        where toss b = fromConstant <$> chooseInteger (0, 2 ^ b - 1)

reverseEndianness' :: forall wordSize k m x {n}.
    ( KnownNat wordSize
    , n ~ k * wordSize
    , m * 8 ~ wordSize
    ) => Vector n x -> Vector n x
reverseEndianness' v =
    let chunks = V.chunks @k @wordSize v
        chunks' = fmap (V.concat . V.reverse . V.chunks @m @8) chunks
     in V.concat chunks'

reverseEndianness :: forall wordSize k c m {n}.
    ( Symbolic c
    , KnownNat wordSize
    , n ~ k * wordSize
    , m * 8 ~ wordSize
    ) => ByteString n c -> ByteString n c
reverseEndianness (ByteString v) = ByteString $ hmap (reverseEndianness' @wordSize @k) v

instance (Symbolic c, KnownNat n) => BoolType (ByteString n c) where
    false = fromConstant (0 :: Natural)
    true = not false

    not (ByteString bits) = ByteString $ fromCircuitF bits $ mapM (\i -> newAssigned (\p -> one - p i))

    l || r = bitwiseOperation l r cons
        where
            cons i j x =
                        let xi = x i
                            xj = x j
                        in xi + xj - xi * xj

    l && r = bitwiseOperation l r cons
        where
            cons i j x =
                        let xi = x i
                            xj = x j
                        in xi * xj

    xor (ByteString l) (ByteString r) =
        ByteString $ symbolic2F l r
            (\x y -> V.unsafeToVector $ fromConstant <$> toBsBits (vecToNat x `B.xor` vecToNat y) (value @n))
            (\lv rv -> do
                let varsLeft = lv
                    varsRight = rv
                zipWithM  (\i j -> newAssigned $ cons i j) varsLeft varsRight
            )
            where
                vecToNat :: (ToConstant a, Const a ~ Natural) => Vector n a -> Natural
                vecToNat = Haskell.foldl (\x p -> toConstant p + 2 * x :: Natural) 0

                cons i j x =
                    let xi = x i
                        xj = x j
                     in xi + xj - (xi * xj + xi * xj)

orRight
    :: forall m n c
    .  Symbolic c
    => ByteString m c
    -> ByteString n c
    -> ByteString (Max m n) c
orRight l r = bitwiseOperation l r cons
        where
            cons i j x =
                        let xi = x i
                            xj = x j
                        in xi + xj - xi * xj

-- | A ByteString of length @n@ can only be split into words of length @wordSize@ if all of the following conditions are met:
-- 1. @wordSize@ is not greater than @n@;
-- 2. @wordSize@ is not zero;
-- 3. The bytestring is not empty;
-- 4. @wordSize@ divides @n@.
--

toWords :: forall m wordSize c. (Symbolic c, KnownNat wordSize) => ByteString (m * wordSize) c -> Vector m (ByteString wordSize c)
toWords (ByteString bits) = ByteString <$> unpackWith (V.chunks @m @wordSize) bits

concat :: forall k m c. (Symbolic c) => Vector k (ByteString m c) -> ByteString (k * m) c
concat bs = ByteString $ packWith V.concat ((\(ByteString bits) -> bits) <$> bs)

-- | Describes types that can be truncated by dropping several bits from the end (i.e. stored in the lower registers)
--

truncate :: forall m n c. (
    Symbolic c
  , KnownNat n
  , n <= m
  ) => ByteString m c -> ByteString n c
truncate (ByteString bits) = ByteString $ hmap (V.take @n) bits

dropN :: forall n m c.
    ( Symbolic c
    , KnownNat (m-n)
    , n <= m
    ) => ByteString m c -> ByteString n c
dropN (ByteString bits) = withDict (plusMinusInverse3 @n @m) $ ByteString $ hmap (V.drop @(m-n)) bits

append
    :: forall m n c
    .  Symbolic c
    => KnownNat m
    => KnownNat n
    => ByteString m c
    -> ByteString n c
    -> ByteString (m + n) c
append (ByteString bits1) (ByteString bits2) =
    ByteString $ fromCircuit2F bits1 bits2 $ \v1 v2 -> pure $ v1 `V.append` v2

--------------------------------------------------------------------------------
instance (Symbolic c, KnownNat n) => ShiftBits (ByteString n c) where
    shiftBits bs@(ByteString oldBits) s
      | s == 0 = bs
      | Haskell.abs s >= Haskell.fromIntegral (getNatural @n) = false
      | otherwise = ByteString $ symbolicF oldBits
          (\v ->  V.shift v s (fromConstant (0 :: Integer)))
          (\bitsV -> do
              let bits = V.fromVector bitsV
              z <- newAssigned (Haskell.const zero)
              let zeros = Haskell.replicate (Haskell.fromIntegral $ Haskell.abs s) z

              let newBits = case s < 0 of
                          Haskell.True  -> take (Haskell.fromIntegral $ getNatural @n) $ zeros <> bits
                          Haskell.False -> drop (Haskell.fromIntegral s) $ bits <> zeros

              pure $ V.unsafeToVector newBits
          )

    rotateBits (ByteString bits) s = ByteString $ hmap (`V.rotate` s) bits

instance
  ( Symbolic c
  , KnownNat k
  , KnownNat n
  ) => Resize (ByteString k c) (ByteString n c) where
    resize (ByteString oldBits)
      | diff > 0 = ByteString $ symbolicF oldBits
          (\v -> V.unsafeToVector $ zeroA <> V.fromVector v)
          (\bitsV -> do
              let bits = V.fromVector bitsV
              zeros <- replicateM diff $ newAssigned (Haskell.const zero)
              return $ V.unsafeToVector $ zeros <> bits
          )
      | otherwise = ByteString $ hmap (V.unsafeToVector . Haskell.drop (Haskell.abs diff) . V.fromVector) oldBits
        where
            diff :: Haskell.Int
            diff = Haskell.fromIntegral (getNatural @n) Haskell.- Haskell.fromIntegral (getNatural @k)

            zeroA = Haskell.replicate diff (fromConstant (0 :: Integer ))

instance
  ( Symbolic c
  , KnownNat n
  ) => SymbolicInput (ByteString n c) where

    isValid (ByteString bits) = Bool $ fromCircuitF bits $ \v -> do
        let vs = V.fromVector v
        ys <- for vs $ \i -> newAssigned (\p -> p i * (one - p i))
        us <-for ys $ \i -> isZero $ Par1 i
        case us of
            []       -> Par1 <$> newAssigned (const one)
            (b : bs) -> foldlM (\(Par1 v1) (Par1 v2) -> Par1 <$> newAssigned (($ v1) * ($ v2))) b bs

set :: forall c n. (Symbolic c, KnownNat n) => ByteString n c -> Natural -> ByteString n c
set (ByteString bits) ix = ByteString $ fromCircuitF bits $ V.mapMWithIx (\i v -> if i == ix then newAssigned (const one) else pure v)

unset :: forall c n. (Symbolic c, KnownNat n) => ByteString n c -> Natural -> ByteString n c
unset (ByteString bits) ix = ByteString $ fromCircuitF bits $ V.mapMWithIx (\i v -> if i == ix then newAssigned (const zero) else pure v)

isSet :: forall c n. Symbolic c => ByteString n c -> Natural -> Bool c
isSet (ByteString bits) ix = Bool $ fromCircuitF bits $ \v -> do
    let vs = V.fromVector v
    return $ Par1 $ (!! ix) vs

isUnset :: forall c n. Symbolic c => ByteString n c -> Natural -> Bool c
isUnset (ByteString bits) ix = Bool $ fromCircuitF bits $ \v -> do
    let vs = V.fromVector v
        i = (!! ix) vs
    j <- newAssigned $ \p -> one - p i
    return $ Par1 j

--------------------------------------------------------------------------------

toBsBits :: Natural -> Natural -> [Natural]
toBsBits num n = reverse bits
    where
        base = 2

        availableBits = unfoldr (toBase base) (num `Haskell.mod` (2 Haskell.^ n)) <> Haskell.repeat 0

        bits = take (Haskell.fromIntegral n) availableBits

-- | Convert a number into @base@-ary system.

toBase :: Natural -> Natural -> Maybe (Natural, Natural)
toBase _ 0    = Nothing
toBase base b = let (d, m) = b `divMod` base in Just (m, d)



-- | A generic bitwise operation on two ByteStrings.
-- If one of the strings is longer, the operation is applied to the least significant bits. The remaining bits are not affected, i.e.
-- 101 || 01001 == 01101
-- TODO: Shall we expose it to users? Can they do something malicious having such function? AFAIK there are checks that constrain each bit to 0 or 1.
--
bitwiseOperation
    :: forall m n c
    .  Symbolic c
    => ByteString m c
    -> ByteString n c
    -> (forall i. i -> i -> ClosedPoly i (BaseField c))
    -> ByteString (Max m n) c
bitwiseOperation (ByteString bits1) (ByteString bits2) cons =
    ByteString $ fromCircuit2F bits1 bits2 $ \lv rv -> do
        let aligned = V.alignRight lv rv
        forM aligned $ \case
            These i j -> newAssigned $ cons i j
            This i -> pure i
            That j -> pure j

instance (Symbolic c, NumberOfBits (BaseField c) ~ n) => Iso (FieldElement c) (ByteString n c) where
  from = ByteString . binaryExpansion

instance (Symbolic c, NumberOfBits (BaseField c) ~ n) => Iso (ByteString n c) (FieldElement c) where
  from (ByteString a) = fromBinary a

instance (Symbolic c, KnownNat n)
    => FromJSON (ByteString n c) where
    parseJSON val = do
        str <- parseJSON val
        case hexToByteString @c @n str of
            Nothing -> Haskell.fail "bad bytestring!"
            Just a  -> return a

instance Arithmetic a => ToJSON (ByteString n (Interpreter a)) where
    toJSON = toJSON . byteStringToHex

byteStringToHex :: Arithmetic a => ByteString n (Interpreter a) -> Haskell.String
byteStringToHex bytes = showHex (toConstant bytes) ""

hexToByteString :: (Symbolic c, KnownNat n) => Haskell.String -> Maybe (ByteString n c)
hexToByteString str = case readHex str of
    [(n, "")] -> Just (fromConstant @Natural n)
    _         -> Nothing
