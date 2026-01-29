{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.JWT.Utils where

import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import Data.Constraint (Dict (..), unmapDict, (:-) (..), (\\))
import Data.Constraint.Nat
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Prelude (fmap, ($), (.), (<$>), type (~))
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Bool
import ZkFold.Data.Eq ((==))
import ZkFold.Data.Ord ((<=))
import ZkFold.Data.Summoner (NumExpr (..), summon)
import ZkFold.Data.Vector (empty, (.:))
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.ByteString (ByteString (..), beBSToReg, concat, regToBSbe, resize, toWords)
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Symbolic.Data.Register hiding (feToReg)
import ZkFold.Symbolic.Data.VarByteString (VarByteString (..), wipeUnassigned)
import qualified ZkFold.Symbolic.Data.VarByteString as VB

-- | The lowest number of bits to store the padded length of a bytestring of @n@ bits
type BufLen n = Max (Log2 n + 1) 3

-- | The smallest multiple of 6 not less than @n@
type Next6 (n :: Natural) = (Div (n + 5) 6) * 6

-- | The number of bits required to store a base64 representation as an ASCII string
-- Each symbol in base64 requires 6 bits, but in ASCII it takes 8 bits, hence the ratio of 8/6
type ASCII (n :: Natural) = (Div n 6) * 8

---------------------------------------------------------------------------------------------------
-- Helper axioms
---------------------------------------------------------------------------------------------------

knownBufLen :: forall n. (KnownNat n, 1 <= n) :- KnownNat (BufLen n)
knownBufLen = unmapDict \Dict ->
  Dict \\ summon @((NLog2 (NConst n) :+ NConst 1) `NMax` NConst 3)

knownNext6 :: forall n. KnownNat n :- KnownNat (Next6 n)
knownNext6 = unmapDict \Dict ->
  Dict \\ summon @(((NConst n :+ NConst 5) :/ NConst 6) :* NConst 6)

knownAscii :: forall n. KnownNat n :- KnownNat (ASCII n)
knownAscii = unmapDict \Dict ->
  Dict \\ summon @((NConst n :/ NConst 6) :* NConst 8)

bufLenPos :: forall n. Dict (1 <= BufLen n)
bufLenPos =
  Dict
    \\ leTrans @1 @3 @(BufLen n)
    \\ maxMonotone1 @0 @(Log2 n + 1) @3
    \\ zeroLe @(Log2 n + 1)

next6Pos :: forall n. (1 <= n) :- (1 <= Next6 n)
next6Pos = unmapDict \Dict ->
  Dict
    \\ leTrans @1 @6 @(Next6 n)
    \\ timesMonotone1 @1 @(Div (n + 5) 6) @6
    \\ divMonotone1 @6 @(n + 5) @6
    \\ plusMonotone1 @1 @n @5

monoAdd :: forall a b c. (a <= b) :- (a <= (c + b))
monoAdd = unmapDict \Dict ->
  Dict
    \\ leTrans @a @(c + a) @(c + b)
    \\ plusMonotone1 @0 @c @a
    \\ zeroLe @c
    \\ plusMonotone2 @c @a @b

divMul :: forall a b. (Mod a b ~ 0, 1 <= b) :- (Div a b * b ~ a)
divMul = unmapDict \Dict ->
  Dict
    \\ timesCommutes @b @(Div a b)
    \\ euclideanNat @b @a

mulMod :: forall n. Dict (Mod (Next6 n) 6 ~ 0)
mulMod =
  Dict
    \\ dividesDef @6 @(Next6 n)
    \\ gcdOne @(Div (n + 5) 6)
    \\ timesDistributesOverGcd @6 @1 @(Div (n + 5) 6)
    \\ timesCommutes @6 @(Div (n + 5) 6)

---------------------------------------------------------------------------------------------------

feToReg :: forall n ctx. FieldElement ctx -> Register (BufLen n) ctx
feToReg (FieldElement c) = MkRegister c

-- | The smallest multiple of 6 not less than the given UInt
padTo6
  :: forall n ctx
   . (KnownNat n, 1 <= n, Symbolic ctx)
  => Register (BufLen n) ctx -> FieldElement ctx
padTo6 ui =
  let v =
        modR @_ @(BufLen n) (extendR @4 ui) (constant @6)
          \\ plusAssociates @(BufLen n) @3 @1
          \\ plusCommutes @3 @(BufLen n)
          \\ knownBufLen @n
   in case toList $ bitsOfR (constant @6 .-. v) of
        [b0, b1, b2] ->
          let f = not (b1 && b2)
           in regToFE $ fromBinaryR $ b0 .: (f && b1) .: (f && b2) .: empty
        _ -> P.error "padTo6: impossible" -- TODO cons-lists rep for vectors

-- | Increase capacity of a VarByteString and increase its length to the nearest multiple of 6
-- Required for base64 encoding.
padBytestring6
  :: forall n ctx
   . (Symbolic ctx, KnownNat n, 1 <= n)
  => VarByteString n ctx -> VarByteString (Next6 n) ctx
padBytestring6 VarByteString {..} =
  VarByteString
    (bsLength + mod6)
    (VB.shiftL newBuf mod6 \\ next6Pos @n \\ knownNext6 @n)
 where
  mod6 = padTo6 @n $ feToReg @n bsLength
  newBuf :: ByteString (Next6 n) ctx
  newBuf = resize bsBuffer \\ knownNext6 @n

-- | Convert a base64-encoded string into an ASCII-encoded string
-- It is expected that both capacity and length of the input bytestring are divisible by 6
-- If either of them is not, apply @padBytestring6@ first.
base64ToAscii
  :: forall n ctx
   . (Symbolic ctx, KnownNat n, 6 <= n, Mod n 6 ~ 0)
  => VarByteString n ctx -> VarByteString (ASCII n) ctx
base64ToAscii VarByteString {..} =
  wipeUnassigned (VarByteString newLen result)
    \\ knownAscii @n
    \\ leTrans @1 @8 @(Div n 6 * 8)
    \\ timesMonotone1 @1 @(Div n 6) @8
    \\ divMonotone1 @6 @n @6
 where
  words6 = toWords @(Div n 6) @6 bsBuffer \\ divMul @n @6
  ascii = word6ToAscii <$> words6

  result :: ByteString (ASCII n) ctx
  result = concat ascii

  newLen :: FieldElement ctx
  newLen =
    scale
      (4 :: Natural)
      ( regToFE $
          divR @_ @(BufLen n)
            (extendR @3 $ feToReg @n bsLength)
            (constant @3)
      )
      \\ plusAssociates @(BufLen n) @2 @1
      \\ plusCommutes @(BufLen n) @2
      \\ knownBufLen @n
      \\ leTrans @1 @6 @n

{-
    Symbols  |  Base64url  |  ASCII
    A..Z         0..25       65..90
    a..z         26..51      97..122
    0..9         52..61      48..57
    -            62          45
    _            63          95
-}
word6ToAscii :: Symbolic ctx => ByteString 6 ctx -> ByteString 8 ctx
word6ToAscii (beBSToReg -> reg) =
  regToBSbe $
    ifThenElse (reg <= extendR (constant @25)) (extendR reg .+. constant @65) $
      ifThenElse (reg <= constant @51) (extendR reg .+. constant @(97 - 26)) $
        ifThenElse (reg <= constant @61) (extendR reg .-. extendR (constant @(61 - 57))) $
          ifThenElse (reg == constant @62) (extendR reg .-. extendR (constant @(62 - 45))) $
            extendR (reg .+. constant @(95 - 63))

-- We store everything as ByteStrings for simplicity.
-- We need to convert ints and bools to strings to avoid conversion errors
stringify :: JSON.Value -> JSON.Value
stringify (JSON.Number s) =
  JSON.String
    (T.pack . P.show . fromMaybe (P.error "instance FromJSON JWT :: Invalid integer") . toBoundedInteger @P.Int $ s)
stringify (JSON.Bool b) = JSON.String (T.pack $ P.show b)
stringify (JSON.Object o) = JSON.Object $ fmap stringify o
stringify a@(JSON.Array _) = JSON.String $ decodeUtf8 . BS.toStrict . JSON.encode $ a
stringify rest = rest
