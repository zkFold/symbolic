{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.VarByteString (
  VarByteString (..),
  fromNatural,
  fromByteString,
  toAsciiString,
  append,
  fromType,
  (@+),
  shiftL,
  shiftR,
  wipeUnassigned,
  dropZeros,
  fromString,
) where

import Control.Applicative (pure)
import Data.Aeson (FromJSON (..))
import qualified Data.ByteString as Bytes
import Data.Char (chr)
import Data.Constraint
import Data.Constraint.Nat
import Data.Constraint.Unsafe
import Data.Function (const)
import Data.Functor (fmap)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits
import Test.QuickCheck (Arbitrary (..))
import Prelude (otherwise, ($), (.), (<$>), type (~))
import qualified Prelude as Haskell

import ZkFold.Algebra.Class
import ZkFold.Algebra.Field
import ZkFold.Algebra.Number
import ZkFold.Control.Conditional (bool, ifThenElse)
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.Summoner (NumExpr (..), Summon (summon))
import ZkFold.Data.Vector (chunks, fromVector)
import qualified ZkFold.Data.Vector as V
import ZkFold.Prelude (chooseNatural, drop)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (fromBool)
import ZkFold.Symbolic.Data.ByteString hiding (append)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.Ord ((<))
import ZkFold.Symbolic.Data.Register (bitsOfFE)
import ZkFold.Symbolic.Data.UIntData

-- | A ByteString that has length unknown at compile time but guaranteed to not exceed @maxLen@.
-- The unassigned buffer space (i.e. bits past @bsLength@) should be set to zero at all times.
--
-- TODO: Declare all the instances ByteString has for VarByteString
data VarByteString (maxLen :: Natural) (context :: Type)
  = VarByteString
  { bsLength :: FieldElement context
  , bsBuffer :: ByteString maxLen context
  }
  deriving (Eq, Generic, Generic1, SymbolicData)

instance (KnownNat maxLen, Symbolic ctx) => Arbitrary (VarByteString maxLen ctx) where
  arbitrary = do
    nbits <- chooseNatural (0, value @maxLen)
    bits <- chooseNatural (0, 2 ^ nbits -! 1)
    pure (fromNatural nbits bits)

toAsciiString
  :: forall n p
   . (KnownNat (Div n 8), (Div n 8) * 8 ~ n, KnownNat p)
  => VarByteString n (Zp p) -> Haskell.String
toAsciiString VarByteString {..} = drop numZeros $ fromVector chars
 where
  ByteString v = bsBuffer
  FieldElement len = bsLength
  strLen = fromZp len `div` 8
  chars =
    chr
      . Haskell.fromIntegral
      . fromZp
      . Haskell.foldl (\b a -> b + b + fromBool a) zero
      <$> chunks @(Div n 8) @8 v
  numZeros = value @(Div n 8) -! strLen

instance
  ( Symbolic ctx
  , m * 8 ~ n
  , KnownNat m
  )
  => IsString (VarByteString n ctx)
  where
  fromString = fromConstant . fromString @Bytes.ByteString

instance
  ( Symbolic ctx
  , m * 8 ~ n
  , KnownNat m
  )
  => FromConstant Bytes.ByteString (VarByteString n ctx)
  where
  fromConstant bytes = VarByteString (fromConstant @Natural . (* 8) . Haskell.fromIntegral $ Bytes.length bytes) (fromConstant bytes)

instance (Symbolic ctx, KnownNat n) => FromConstant Natural (VarByteString n ctx) where
  fromConstant 0 = VarByteString zero (fromConstant @Natural 0)
  fromConstant n = VarByteString (fromConstant $ Haskell.min (value @n) (ilog2 n + 1)) (fromConstant n)

fromNatural :: forall n ctx. (Symbolic ctx, KnownNat n) => Natural -> Natural -> VarByteString n ctx
fromNatural numBits n = VarByteString (fromConstant numBits) (fromConstant n)

fromByteString :: forall n ctx. (Symbolic ctx, KnownNat n) => ByteString n ctx -> VarByteString n ctx
fromByteString = VarByteString (fromConstant $ value @n)

instance (Symbolic ctx, KnownNat m, m * 8 ~ n) => FromJSON (VarByteString n ctx) where
  parseJSON v = fromString <$> parseJSON v

type family Length' (s :: Haskell.Maybe (Haskell.Char, Symbol)) :: Natural where
  Length' 'Haskell.Nothing = 0
  Length' ('Haskell.Just '(c, rest)) = 1 + Length' (UnconsSymbol rest)

type family Length (s :: Symbol) :: Natural where
  Length s = Length' (UnconsSymbol s)

-- | Construct a VarByteString from a type-level string calculating its length automatically
fromType
  :: forall s ctx
   . (Symbolic ctx, KnownSymbol s, KnownNat (Length s))
  => VarByteString (Length s * 8) ctx
fromType = fromString $ symbolVal (Proxy @s)

monoMax :: forall (m :: Natural) (n :: Natural). Dict (Max (m + n) n ~ (m + n))
monoMax = unsafeAxiom

withMax :: forall (m :: Natural) (n :: Natural) {r}. (Max (m + n) n ~ (m + n) => r) -> r
withMax = withDict (monoMax @m @n)

-- | Join two variable-length ByteStrings and move all the unsaaigned space towards lower indices.
-- Let @u@ denote the unassigned space. Then,
-- uu1010 `append` u10010 == uuu101010010
append
  :: forall m n ctx
   . Symbolic ctx
  => KnownNat m
  => (1 <= m + n, KnownNat (m + n))
  => VarByteString m ctx
  -> VarByteString n ctx
  -> VarByteString (m + n) ctx
append (VarByteString l1 bs1) (VarByteString l2 bs2) = VarByteString (l1 + l2) $ withMax @m @n newBs
 where
  ex1 :: ByteString (m + n) ctx
  ex1 = resize bs1

  newBs = (ex1 `shiftL` l2) `orRight` bs2

infixl 6 @+

(@+)
  :: forall m n ctx
   . Symbolic ctx
  => KnownNat m
  => (KnownNat (m + n), 1 <= m + n)
  => VarByteString m ctx
  -> VarByteString n ctx
  -> VarByteString (m + n) ctx
(@+) = append

shift
  :: forall n ctx
   . Symbolic ctx
  => (1 <= n, KnownNat n)
  => (Words n ctx -> Natural -> Words n ctx)
  -> ByteString n ctx
  -> FieldElement ctx
  -> ByteString n ctx
shift sh bs el =
  wordsToBS $
    Haskell.foldr
      ( \s b ->
          bool b (b `sh` (2 ^ s)) $
            withDict (log2Nat @n) $
              withDict (plusNat @(Log2 n) @1) $
                isSet elBits s
      )
      w
      [0 .. nbits]
 where
  elBits :: ByteString (Log2 n + 1) ctx
  elBits =
    ByteString $
      V.take (bitsOfFE el)
        \\ summon @(NLog2 (NConst n) :+ NConst 1)
  w :: Words n ctx
  w = bsToWords bs

  -- No need to perform more shifts than this.
  -- The bytestring will be all zeros beyond this iteration.
  nbits = ilog2 $ value @n

shiftL
  :: forall n ctx
   . Symbolic ctx
  => (1 <= n, KnownNat n)
  => ByteString n ctx
  -> FieldElement ctx
  -> ByteString n ctx
shiftL = shift shiftWordsL

shiftR
  :: forall n ctx
   . Symbolic ctx
  => (1 <= n, KnownNat n)
  => ByteString n ctx
  -> FieldElement ctx
  -> ByteString n ctx
shiftR = shift shiftWordsR

-- | Set all the unassigned bits to zero
wipeUnassigned
  :: forall n ctx
   . Symbolic ctx
  => (1 <= n, KnownNat n)
  => VarByteString n ctx -> VarByteString n ctx
wipeUnassigned VarByteString {..} = VarByteString bsLength ((`shiftR` unassigned) . (`shiftL` unassigned) $ bsBuffer)
 where
  unassigned :: FieldElement ctx
  unassigned = fromConstant (value @n) - bsLength

-----------------------------------------------------------------------------------------------------------------------
-- Helper types and functions for internal usage.
-- They optimise shifting by working with words rather than bits
-----------------------------------------------------------------------------------------------------------------------

type WordSize c = Div (NumberOfBits c) 2

newtype Words n c = Words {runWords :: UIntData n (WordSize c) c}

log2Monotone :: (1 <= m, m <= n) :- (Log2 m <= Log2 n)
log2Monotone = unmapDict (const unsafeAxiom)

minusMonotone :: (l <= m, m <= n) :- (m - l <= n - l)
minusMonotone = unmapDict (const unsafeAxiom)

knownWordsData
  :: forall n c. (KnownNat n, Symbolic c) :- KnownUIntData n (WordSize c)
knownWordsData = unmapDict \Dict ->
  Dict
    \\ knownUIntData @n @(WordSize c)
    \\ divNat @(NumberOfBits c) @2
    \\ divMonotone1 @2 @(NumberOfBits c) @2
    \\ plusMonotone1 @1 @(Log2 (Order c - 1)) @1
    \\ log2Monotone @2 @(Order c - 1)
    \\ minusMonotone @1 @3 @(Order c)

instance SymbolicData (Words n) where
  type Layout (Words n) c = Layout (UIntData n (WordSize c)) c
  type HasRep (Words n) c = HasRep (UIntData n (WordSize c)) c

  toLayout = toLayout . runWords
  interpolate c = Words . interpolate c . fmap (runWords <$>)
  fromLayout = Words . fromLayout

wordsToBS :: forall n c. (Symbolic c, KnownNat n) => Words n c -> ByteString n c
wordsToBS = uintDataToBSbe . runWords \\ knownWordsData @n @c

bsToWords :: forall n c. (Symbolic c, KnownNat n) => ByteString n c -> Words n c
bsToWords = Words . beBSToUIntData \\ knownWordsData @n @c

-- | shift a vector of words left by a power of two
shiftWordsL
  :: forall n ctx
   . Symbolic ctx
  => KnownNat n
  => Words n ctx -> Natural -> Words n ctx
shiftWordsL (Words regs) p2
  | p2 Haskell.>= value @n = Words zero \\ knownWordsData @n @ctx
  | p2 Haskell.== 0 = Words regs
  | otherwise =
      Words $
        shiftUIntData regs (fromConstant p2) \\ knownWordsData @n @ctx

shiftWordsR
  :: forall n ctx
   . Symbolic ctx
  => KnownNat n
  => Words n ctx -> Natural -> Words n ctx
shiftWordsR (Words regs) p2
  | p2 Haskell.>= value @n = Words zero \\ knownWordsData @n @ctx
  | p2 Haskell.== 0 = Words regs
  | otherwise =
      Words $
        shiftUIntData regs (-fromConstant p2) \\ knownWordsData @n @ctx

dropZeros :: forall n m c. (Symbolic c, KnownNat n, n <= m, KnownNat (m - n)) => VarByteString m c -> VarByteString n c
dropZeros VarByteString {..} = ifThenElse (bsLength < feN) bsNMoreLen bsNLessLen
 where
  feN = fromConstant (value @n)
  bsNMoreLen = VarByteString bsLength (dropN bsBuffer)
  bsNLessLen = VarByteString feN (truncate bsBuffer)
