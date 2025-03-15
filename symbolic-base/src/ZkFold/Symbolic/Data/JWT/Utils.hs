{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.JWT.Utils where

import           Control.DeepSeq                    (NFData, force)
import qualified Data.Aeson                         as JSON
import qualified Data.ByteString                    as BS
import           Data.Constraint                    (Dict (..), withDict, (:-) (..))
import           Data.Constraint.Nat                (Max, divNat, minusNat, plusNat, timesNat)
import           Data.Constraint.Unsafe             (unsafeAxiom, unsafeSNat)
import           Data.Maybe                         (fromMaybe)
import           Data.Scientific                    (toBoundedInteger)
import qualified Data.Text                          as T
import           Data.Text.Encoding                 (decodeUtf8)
import           GHC.Generics                       (Par1 (..))
import           GHC.TypeLits                       (withKnownNat)
import           Prelude                            (fmap, pure, type (~), ($), (.), (<$>))
import qualified Prelude                            as P

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Data.HFunctor          (hmap)
import qualified ZkFold.Base.Data.Vector            as V
import           ZkFold.Base.Data.Vector            ((!!))
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.ByteString    (ByteString (..), concat, toWords)
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.FieldElement
import           ZkFold.Symbolic.Data.UInt
import qualified ZkFold.Symbolic.Data.VarByteString as VB
import           ZkFold.Symbolic.Data.VarByteString (VarByteString (..), wipeUnassigned)
import           ZkFold.Symbolic.MonadCircuit       (newAssigned)

-- | The lowest number of bits to store the padded length of a bytestring of @n@ bits
--
type BufLen n = Max (Log2 n + 1) 3

-- | The smallest multiple of 6 not less than @n@
--
type Next6 (n :: Natural) = (Div (n + 5) 6) * 6

-- | The number of bits required to store a base64 representation as an ASCII string
-- Each symbol in base64 requires 6 bits, but in ASCII it takes 8 bits, hence the ratio of 8/6
--
type ASCII (n :: Natural) = (Div n 6) * 8

---------------------------------------------------------------------------------------------------
    -- Helper axioms
---------------------------------------------------------------------------------------------------

knownBufLen' :: forall n . KnownNat n :- KnownNat (BufLen n)
knownBufLen' = Sub $ withKnownNat @(BufLen n) (unsafeSNat $ P.max (ilog2 (value @n) + 1) 3) Dict

knownBufLen :: forall n {r} . KnownNat n => (KnownNat (BufLen n) => r) -> r
knownBufLen = withDict (knownBufLen' @n)

monoAdd :: forall (a :: Natural) (b :: Natural) (c :: Natural) . (a <= b) :- (a <= (c + b))
monoAdd = Sub unsafeAxiom

oneReg :: forall n c . Dict (NumberOfRegisters (BaseField c) (BufLen n) ('Fixed (BufLen n)) ~ 1)
oneReg = unsafeAxiom -- @BufLen n@ is always greater than 2

knownOneReg' :: forall n c . Dict (KnownNat (NumberOfRegisters (BaseField c) (BufLen n) ('Fixed (BufLen n))))
knownOneReg' = withKnownNat @(NumberOfRegisters (BaseField c) (BufLen n) ('Fixed (BufLen n))) (unsafeSNat 1) Dict

knownOneReg :: forall n c {r} . (KnownNat (NumberOfRegisters (BaseField c) (BufLen n) ('Fixed (BufLen n))) => r) -> r
knownOneReg = withDict (knownOneReg' @n @c)

knownNumWords' :: forall n c . KnownNat n :- KnownNat (Div (GetRegisterSize (BaseField c) (BufLen n) ('Fixed (BufLen n)) + OrdWord - 1) OrdWord)
knownNumWords' = Sub $
    withKnownNat @(Div (GetRegisterSize (BaseField c) (BufLen n) ('Fixed (BufLen n)) + OrdWord - 1) OrdWord)
        (unsafeSNat $ knownBufLen @n $ wordSize (value @(BufLen n)))
        Dict
    where
        wordSize :: Natural -> Natural
        wordSize n = (n + (value @OrdWord) -! 1) `div` (value @OrdWord)

knownNumWords :: forall n c {r} . KnownNat n => (KnownNat (Div (GetRegisterSize (BaseField c) (BufLen n) ('Fixed (BufLen n)) + OrdWord - 1) OrdWord) => r) -> r
knownNumWords = withDict (knownNumWords' @n @c)

withDiv :: forall n {r}. KnownNat n => (KnownNat (Div ((n + OrdWord) - 1) OrdWord) => r) -> r
withDiv =
    withDict (plusNat @n @OrdWord) $
        withDict (monoAdd @1 @OrdWord @n) $
            withDict (minusNat @(n + OrdWord) @1) $
                withDict (divNat @(n + OrdWord - 1) @OrdWord)

withNext6 :: forall n {r}. KnownNat n => (KnownNat (Next6 n) => r) -> r
withNext6 =
    withDict (plusNat @n @5) $
        withDict (divNat @(n + 5) @6) $
            withDict (timesNat @(Div (n + 5) 6) @6)

withAscii :: forall n {r}. KnownNat n => (KnownNat (ASCII n) => r) -> r
withAscii =
    withDict (divNat @n @6) $
        withDict (timesNat @(Div n 6) @8)

divMul :: forall a b . (Mod a b ~ 0) :- ((Div a b) * b ~ a)
divMul = Sub unsafeAxiom

mulMod :: forall n . Dict (Mod (Next6 n) 6 ~ 0)
mulMod = unsafeAxiom

withDivMul :: forall a b {r}. (Mod a b ~ 0) => ((Div a b) * b ~ a => r) -> r
withDivMul = withDict (divMul @a @b)

---------------------------------------------------------------------------------------------------

feToUInt :: forall n ctx . Symbolic ctx => FieldElement ctx -> UInt (BufLen n) ('Fixed (BufLen n)) ctx
feToUInt (FieldElement c) = UInt $ withDict (oneReg @n @ctx) $ hmap (V.singleton . unPar1) c

uintToFe :: forall n ctx . Symbolic ctx => UInt (BufLen n) ('Fixed (BufLen n)) ctx -> FieldElement ctx
uintToFe (UInt v) = FieldElement $ withDict (oneReg @n @ctx) $ hmap (Par1 . V.item) v

-- | The smallest multiple of 6 not less than the given UInt
--
padTo6
    :: forall n ctx
    .  Symbolic ctx
    => KnownNat n
    => UInt (BufLen n) ('Fixed (BufLen n)) ctx
    -> FieldElement ctx
padTo6 ui = FieldElement $ fromCircuitF v $ \bits ->
    do
        val <- horner $ V.fromVector bits

        toPad <- newAssigned $ \p -> fromConstant @Natural 6 - p val
        valBits <- V.unsafeToVector @3 <$> expansion 3 toPad

        f <- newAssigned $ \p -> one - p (valBits !! 1) * p (valBits !! 2)
        hi1 <- newAssigned $ \p -> p f * p (valBits !! 1)
        hi2 <- newAssigned $ \p -> p f * p (valBits !! 2)
        res <- horner [valBits !! 0, hi1, hi2]

        pure $ Par1 res
    where
        UInt v =
            knownBufLen @n $
                knownNumWords @n @ctx $
                    knownOneReg @n @ctx $
                        withDiv @(BufLen n) $
                            ui `mod` (fromConstant @Natural 6)


-- | Increase capacity of a VarByteString and increase its length to the nearest multiple of 6
-- Required for base64 encoding.
--
padBytestring6
    :: forall n ctx
    .  Symbolic ctx
    => KnownNat n
    => VarByteString n ctx -> VarByteString (Next6 n) ctx
padBytestring6 VarByteString{..} = VarByteString (bsLength + mod6) (withNext6 @n $ VB.shiftL newBuf mod6)
    where
        mod6 = padTo6 @n $ feToUInt @n bsLength
        newBuf = withNext6 @n $ resize bsBuffer


-- | Convert a base64-encoded string into an ASCII-encoded string
-- It is expected that both capacity and length of the input bytestring are divisible by 6
-- If either of them is not, apply @padBytestring6@ first.
--
base64ToAscii
    :: forall n ctx
    .  Symbolic ctx
    => KnownNat n
    => Mod n 6 ~ 0
    => NFData (ctx (V.Vector 8))
    => NFData (ctx (V.Vector (ASCII n)))
    => VarByteString n ctx -> VarByteString (ASCII n) ctx
base64ToAscii VarByteString{..} = withAscii @n $ wipeUnassigned $ VarByteString newLen result
    where
        words6 = withDivMul @n @6 $ toWords @(Div n 6) @6 bsBuffer
        ascii  = word6ToAscii <$> words6
        result = force $ concat ascii

        newLen =
            knownBufLen @n $
                withDiv @(BufLen n) $
                    knownOneReg @n @ctx $
                        knownNumWords @n @ctx $
                            scale (4 :: Natural) . (uintToFe @n) . (`div` (fromConstant @Natural 3)) . (feToUInt @n) $ bsLength


{-
    Symbols  |  Base64url  |  ASCII
    A..Z         0..25       65..90
    a..z         26..51      97..122
    0..9         52..61      48..57
    -            62          45
    _            63          95
-}
word6ToAscii :: forall ctx . (Symbolic ctx, NFData (ctx (V.Vector 8))) => ByteString 6 ctx -> ByteString 8 ctx
word6ToAscii (ByteString bs) = force $ ByteString $ fromCircuitF bs $ \bits ->
    do
        let bitsSym = V.fromVector bits

        fe <- horner (P.reverse bitsSym)

        z <- newAssigned (P.const zero)
        o <- newAssigned (P.const one)

        let bits25 = [z,o,o,z,z,o]
            bits51 = [o,o,z,z,o,o]
            bits61 = [o,o,o,o,z,o]
            bits62 = [o,o,o,o,o,z]

        isAZ   <- blueprintGE @6 bits25 bitsSym
        leaz   <- blueprintGE @6 bits51 bitsSym
        le09   <- blueprintGE @6 bits61 bitsSym
        ledash <- blueprintGE @6 bits62 bitsSym

        isaz   <- newAssigned $ \p -> p leaz * (one - p isAZ)
        is09   <- newAssigned $ \p -> p le09 * (one - p leaz)
        isdash <- newAssigned $ \p -> p ledash * (one - p le09)
        isus   <- newAssigned $ \p -> one - p ledash

        asciiAZ   <- newAssigned $ \p -> p isAZ   * (p fe + (fromConstant @Natural 65))
        asciiaz   <- newAssigned $ \p -> p isaz   * (p fe + (fromConstant @Natural 71))
        ascii09   <- newAssigned $ \p -> p is09   * (p fe - (fromConstant @Natural 4 ))
        asciidash <- newAssigned $ \p -> p isdash * (p fe - (fromConstant @Natural 17))
        asciius   <- newAssigned $ \p -> p isus   * (p fe + (fromConstant @Natural 32))

        s1 <- newAssigned $ \p -> p asciiAZ   + p asciiaz
        s2 <- newAssigned $ \p -> p ascii09   + p s1
        s3 <- newAssigned $ \p -> p asciidash + p s2
        s4 <- newAssigned $ \p -> p asciius   + p s3

        V.unsafeToVector . P.reverse <$> expansion 8 s4

-- We store everything as ByteStrings for simplicity.
-- We need to convert ints and bools to strings to avoid conversion errors
--
stringify :: JSON.Value -> JSON.Value
stringify (JSON.Number s) =
    JSON.String (T.pack . P.show . fromMaybe (P.error "instance FromJSON JWT :: Invalid integer") . toBoundedInteger @P.Int $ s)
stringify (JSON.Bool b)   = JSON.String (T.pack $ P.show b)
stringify (JSON.Object o) = JSON.Object $ fmap stringify o
stringify a@(JSON.Array _) = JSON.String $ decodeUtf8 . BS.toStrict . JSON.encode $ a
stringify rest            = rest
