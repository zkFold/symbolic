{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.VarByteString
    ( VarByteString (..)
    , fromNatural
    , fromByteString
    , toAsciiString
    , append
    , (@+)
    , shiftL
    , shiftR
    , wipeUnassigned
    , dropZeros
    , fromString
    ) where

import           Control.DeepSeq                   (NFData)
import           Control.Monad                     (mapM)
import           Data.Aeson                        (FromJSON (..))
import qualified Data.ByteString                   as Bytes
import           Data.Char                         (chr)
import           Data.Constraint
import           Data.Constraint.Nat
import           Data.Constraint.Unsafe
import           Data.Foldable                     (foldlM, foldrM)
import           Data.Functor.Rep                  (Representable (..))
import           Data.Kind                         (Type)
import           Data.List.Split                   (chunksOf)
import           Data.Proxy                        (Proxy (..))
import           Data.String                       (IsString (..))
import           GHC.Generics                      (Generic, Par1 (..))
import           GHC.TypeLits                      (KnownSymbol (..), symbolVal, withKnownNat)
import           Prelude                           (const, fmap, otherwise, pure, type (~), ($), (.), (<$>), (<>))
import qualified Prelude                           as Haskell
import           Test.QuickCheck                   (Arbitrary (..), chooseInteger)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Data.Vector           (Vector, chunks, fromVector, unsafeToVector)
import           ZkFold.Prelude                    (drop, length, replicate, take)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool         (Bool (..))
import           ZkFold.Symbolic.Data.ByteString   (ByteString (..), dropN, isSet, orRight, truncate)
import           ZkFold.Symbolic.Data.Class        (SymbolicData)
import           ZkFold.Symbolic.Data.Combinators  hiding (regSize)
import           ZkFold.Symbolic.Data.Conditional  (Conditional, bool, ifThenElse)
import           ZkFold.Symbolic.Data.Eq           (Eq)
import           ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import           ZkFold.Symbolic.Data.Input        (SymbolicInput)
import           ZkFold.Symbolic.Data.Ord          ((<))
import           ZkFold.Symbolic.Interpreter
import           ZkFold.Symbolic.MonadCircuit      (MonadCircuit, newAssigned)

-- | A ByteString that has length unknown at compile time but guaranteed to not exceed @maxLen@.
-- The unassigned buffer space (i.e. bits past @bsLength@) should be set to zero at all times.
--
-- TODO: Declare all the instances ByteString has for VarByteString
--
data VarByteString (maxLen :: Natural) (context :: (Type -> Type) -> Type) =
    VarByteString
        { bsLength :: FieldElement context
        , bsBuffer :: ByteString maxLen context
        }
    deriving (Generic)

deriving stock instance (Haskell.Show (ctx (Vector n)), Haskell.Show (ctx Par1)) => Haskell.Show (VarByteString n ctx)
deriving stock instance (Haskell.Eq (ctx (Vector n)), Haskell.Eq (ctx Par1)) => Haskell.Eq (VarByteString n ctx)
deriving anyclass instance (NFData (ctx (Vector n)), NFData (ctx Par1)) => NFData (VarByteString n ctx)
deriving instance (KnownNat n, Symbolic ctx) => SymbolicData (VarByteString n ctx)
deriving instance (KnownNat n, Symbolic ctx) => SymbolicInput (VarByteString n ctx)
deriving instance (Symbolic ctx, KnownNat n) => Eq (VarByteString n ctx)
deriving instance (Symbolic ctx, KnownNat n) => Conditional (Bool ctx) (VarByteString n ctx)

instance (KnownNat maxLen, Symbolic ctx) => Arbitrary (VarByteString maxLen ctx) where
    arbitrary = do
        nbits <- Haskell.fromIntegral <$> chooseInteger (0, Haskell.fromIntegral $ value @maxLen)
        bits  <- Haskell.fromIntegral <$> chooseInteger (0, Haskell.fromIntegral $ 2^nbits -! 1)
        pure $ fromNatural nbits bits

toAsciiString :: forall n p . (KnownNat (Div n 8), (Div n 8) * 8 ~ n, KnownNat p) => VarByteString n (Interpreter (Zp p)) -> Haskell.String
toAsciiString VarByteString{..} = drop numZeros $ fromVector chars
    where
        ByteString (Interpreter v) = bsBuffer
        FieldElement (Interpreter (Par1 len)) = bsLength
        strLen = fromZp len `div` 8
        chars = chr . Haskell.fromIntegral . fromZp . Haskell.foldl (\b a -> b + b + a) zero <$> chunks @(Div n 8) @8 v
        numZeros = value @(Div n 8) -! strLen

instance
    ( Symbolic ctx
    , m * 8 ~ n
    , KnownNat m
    ) => IsString (VarByteString n ctx) where
    fromString = fromConstant . fromString @Bytes.ByteString

instance
    ( Symbolic ctx
    , m * 8 ~ n
    , KnownNat m
    ) => FromConstant Bytes.ByteString (VarByteString n ctx) where
    fromConstant bytes = VarByteString (fromConstant @Natural . (*8) . Haskell.fromIntegral $ Bytes.length bytes) (fromConstant bytes)

instance (Symbolic ctx, KnownNat n) => FromConstant Natural (VarByteString n ctx) where
    fromConstant 0 = VarByteString zero (fromConstant @Natural 0)
    fromConstant n = VarByteString (fromConstant $ Haskell.min (value @n) (ilog2 n + 1)) (fromConstant n)

fromNatural :: forall n ctx . (Symbolic ctx, KnownNat n) => Natural -> Natural -> VarByteString n ctx
fromNatural numBits n = VarByteString (fromConstant numBits) (fromConstant n)

fromByteString :: forall n ctx . (Symbolic ctx, KnownNat n) => ByteString n ctx -> VarByteString n ctx
fromByteString bs = VarByteString (fromConstant $ value @n) bs

instance (Symbolic ctx, KnownNat m, m * 8 ~ n) => FromJSON (VarByteString n ctx) where
    parseJSON v = fromString <$> parseJSON v

-- | Construct a VarByteString from a type-level string calculating its length automatically
--
instance
    ( Symbolic ctx
    , KnownSymbol s
    , m ~ Length s
    , m * 8 ~ l
    , KnownNat m
    ) => IsTypeString s (VarByteString l ctx) where
    fromType = fromString $ symbolVal (Proxy @s)

monoMax :: forall (m :: Natural) (n :: Natural) . Dict (Max (m + n) n ~ (m + n))
monoMax = unsafeAxiom

withMax :: forall (m :: Natural) (n :: Natural) {r}. ((Max (m + n) n ~ (m + n)) => r) -> r
withMax = withDict (monoMax @m @n)

-- | Join two variable-length ByteStrings and move all the unsaaigned space towards lower indices.
-- Let @u@ denote the unassigned space. Then,
-- uu1010 `append` u10010 == uuu101010010
--
append
    :: forall m n ctx
    .  Symbolic ctx
    => KnownNat m
    => KnownNat (m + n)
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
    .  Symbolic ctx
    => KnownNat m
    => KnownNat (m + n)
    => VarByteString m ctx
    -> VarByteString n ctx
    -> VarByteString (m + n) ctx
(@+) = append

shift
    :: forall n ctx
    .  Symbolic ctx
    => KnownNat n
    => (Words n ctx -> Natural -> Words n ctx)
    -> ByteString n ctx
    -> FieldElement ctx
    -> ByteString n ctx
shift sh bs (FieldElement el) = from $ Haskell.foldr (\s b -> withWordCount @n @ctx $ bool b (b `sh` (2^s)) (isSet elBits s)) w [0 .. nbits]
    where
        elBits :: ByteString (Log2 n + 1) ctx
        elBits = ByteString $ fromCircuitF el $ fmap unsafeToVector . expansion (nbits + 1) . unPar1

        w :: Words n ctx
        w = from bs

        -- No need to perform more shifts than this.
        -- The bytestring will be all zeros beyond this iteration.
        nbits = ilog2 $ value @n

shiftL
    :: forall n ctx
    .  Symbolic ctx
    => KnownNat n
    => ByteString n ctx
    -> FieldElement ctx
    -> ByteString n ctx
shiftL = shift shiftWordsL

shiftR
    :: forall n ctx
    .  Symbolic ctx
    => KnownNat n
    => ByteString n ctx
    -> FieldElement ctx
    -> ByteString n ctx
shiftR = shift shiftWordsR

-- | Set all the unassigned bits to zero
--
wipeUnassigned
    :: forall n ctx
    .  Symbolic ctx
    => KnownNat n
    => VarByteString n ctx -> VarByteString n ctx
wipeUnassigned VarByteString{..} = VarByteString bsLength ((`shiftR` unassigned) . (`shiftL` unassigned) $ bsBuffer)
    where
        unassigned :: FieldElement ctx
        unassigned = fromConstant (value @n) - bsLength


-----------------------------------------------------------------------------------------------------------------------
-- Helper types and functions for internal usage.
-- They optimise shifting by working with words rather than bits
-----------------------------------------------------------------------------------------------------------------------


type WordSize ctx = Div (NumberOfBits (BaseField ctx)) 2

natWordSize :: Natural -> Natural
natWordSize n = (ilog2 (n -! 1) + 1) `div` 2

withWordSize' :: forall a . (KnownNat (Order (BaseField a))) :- KnownNat (WordSize a)
withWordSize' = Sub $ withKnownNat @(WordSize a) (unsafeSNat (natWordSize (value @(Order (BaseField a))))) Dict

withWordSize :: forall a {r}. (KnownNat (Order (BaseField a))) => (KnownNat (WordSize a) => r) -> r
withWordSize = withDict (withWordSize' @a)

type WordCount n ctx = Div (n + WordSize ctx - 1) (WordSize ctx)

withWordCount
    :: forall n ctx {r}
    .  KnownNat n
    => KnownNat (Order (BaseField ctx))
    => (KnownNat (WordCount n ctx) => r) -> r
withWordCount =
    withWordSize @ctx $
        withDict (unsafeAxiom @(1 <= WordSize ctx)) $
            withDict (unsafeAxiom @(1 <= n + WordSize ctx)) $
                withDict (plusNat @n @(WordSize ctx)) $
                    withDict (minusNat @(n + WordSize ctx) @1) $
                        withDict (divNat @(n + WordSize ctx - 1) @(WordSize ctx))

newtype Words n ctx = Words (ctx (Vector (WordCount n ctx)))
    deriving Generic

deriving newtype instance NFData (ctx (Vector (WordCount n ctx))) => NFData (Words n ctx)
deriving newtype instance Haskell.Show (ctx (Vector (WordCount n ctx))) => Haskell.Show (Words n ctx)
deriving newtype instance (KnownNat (WordCount n ctx), Symbolic ctx) => SymbolicData (Words n ctx)
deriving newtype instance (KnownNat (WordCount n ctx), Symbolic ctx) => Conditional (Bool ctx) (Words n ctx)

instance
  ( Symbolic ctx
  , KnownNat n
  ) => Iso (Words n ctx) (ByteString n ctx) where
    from (Words regs) = ByteString $ fromCircuitF regs $ \r -> do
        let v = fromVector r
            (w, ws) = (Haskell.head v, Haskell.tail v)
            regSize = withWordSize @ctx $ value @(WordSize ctx)
            hiRegSize = value @n -! regSize * (withWordCount @n @ctx $ value @(WordCount n ctx) -! 1)
        lows <- Haskell.concatMap Haskell.reverse <$> mapM (expansion regSize) ws
        hi <- Haskell.reverse <$> expansion hiRegSize w
        pure $ unsafeToVector (hi <> lows)

instance
  ( Symbolic ctx
  , KnownNat n
  ) => Iso (ByteString n ctx) (Words n ctx) where
    from (ByteString bits) = Words $ fromCircuitF bits $ \b -> do
        let bs = fromVector b
            regSize = withWordSize @ctx $ value @(WordSize ctx)
            hiRegSize = value @n -! regSize * (withWordCount @n @ctx $ value @(WordCount n ctx) -! 1)
            his = take hiRegSize bs
            lows = chunksOf (Haskell.fromIntegral regSize) $ drop hiRegSize bs
        hi <- horner $ Haskell.reverse his
        lo <- mapM (horner . Haskell.reverse) lows
        pure $ unsafeToVector (hi:lo)


-- | shift a vector of words left by a power of two
--
shiftWordsL
    :: forall n ctx
    .  Symbolic ctx
    => KnownNat n
    => Words n ctx -> Natural -> Words n ctx
shiftWordsL (Words regs) p2
  | p2 Haskell.>= (value @n) = Words $ withWordCount @n @ctx $ embed (tabulate zero)
  | p2 Haskell.== 0 = Words regs
  | otherwise = Words shifted
    where
        regSize :: Natural
        regSize = withWordSize @ctx $ value @(WordSize ctx)

        hiRegSize :: Natural
        hiRegSize = value @n `mod` regSize

        -- How many registers will be empty after the shift
        (zeroRegs, remShift) = p2 `divMod` regSize

        shifted = fromCircuitF regs $ \r -> do
            let lst = fromVector r
                v = drop zeroRegs lst
                (hi, lows) = (Haskell.head v, Haskell.tail v)

            z <- newAssigned (const zero)
            (newRegs, carry) <- foldrM shiftCarry ([], z) lows

            s <- newAssigned $ \p -> scale ((2 :: Natural) ^ remShift) (p hi) + p carry
            (newHi, _) <- splitExpansion hiRegSize regSize s
            pure $ unsafeToVector ((newHi : newRegs) <> replicate zeroRegs z)

        shiftCarry :: MonadCircuit i (BaseField ctx) w m => i -> ([i], i) -> m ([i], i)
        shiftCarry r (acc, carry) = do
            s <- newAssigned $ \p -> scale ((2 :: Natural) ^ remShift) (p r) + p carry
            (l, h) <- splitExpansion regSize regSize s
            pure (l : acc, h)

shiftWordsR
    :: forall n ctx
    .  Symbolic ctx
    => KnownNat n
    => Words n ctx -> Natural -> Words n ctx
shiftWordsR (Words regs) p2
  | p2 Haskell.>= (value @n) = Words $ withWordCount @n @ctx $ embed (tabulate zero)
  | p2 Haskell.== 0 = Words regs
  | otherwise = Words shifted
    where
        regSize :: Natural
        regSize = withWordSize @ctx $ value @(WordSize ctx)

        hiRegSize :: Natural
        hiRegSize = value @n `mod` regSize

        -- How many registers will be empty after the shift
        (zeroRegs, remShift) = p2 `divMod` regSize

        shifted = fromCircuitF regs $ \r -> do
            let lst = fromVector r
                v = take (length lst -! zeroRegs) lst
                (hi, lows) = (Haskell.head v, Haskell.tail v)

            z <- newAssigned (const zero)

            (carry, newHi) <- case (hiRegSize Haskell.> remShift) of
                                Haskell.True -> splitExpansion remShift (hiRegSize -! remShift) hi
                                _            -> pure (hi, z)

            (newRegs, _) <- foldlM shiftCarry ([], carry) lows

            pure $ unsafeToVector (replicate zeroRegs z <> (newHi : Haskell.reverse newRegs))

        shiftCarry :: MonadCircuit i (BaseField ctx) w m => ([i], i) -> i -> m ([i], i)
        shiftCarry (acc, carry) r = do
            (l, h) <- splitExpansion remShift (regSize -! remShift) r
            s <- newAssigned $ \p ->  p h + scale ((2 :: Natural) ^ (regSize -! remShift)) (p carry)
            pure (s : acc, l)

dropZeros :: forall n m c . (Symbolic c, KnownNat n, n <= m, KnownNat (m - n)) => VarByteString m c -> VarByteString n c
dropZeros VarByteString{..} = ifThenElse (bsLength < feN) bsNMoreLen bsNLessLen
    where
        feN = fromConstant (value @n)
        bsNMoreLen = VarByteString bsLength (dropN bsBuffer)
        bsNLessLen = VarByteString feN (truncate bsBuffer)
