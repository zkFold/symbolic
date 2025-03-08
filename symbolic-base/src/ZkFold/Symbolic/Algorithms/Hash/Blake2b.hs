{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -freduction-depth=0 #-} -- Avoid reduction overflow error caused by NumberOfRegisters

module ZkFold.Symbolic.Algorithms.Hash.Blake2b (blake2b_224, blake2b_256, blake2b_512) where

import           Data.Bool                                         (bool)
import           Data.Constraint                                   (Dict, withDict)
import           Data.Constraint.Nat                               (minusNat, modBound, modNat, plusMonotone2, plusNat,
                                                                    timesNat, zeroLe)
import           Data.Constraint.Unsafe                            (unsafeAxiom)
import           Data.Ratio                                        ((%))
import           Data.Vector                                       ((!), (//))
import qualified Data.Vector                                       as V
import           GHC.IsList                                        (IsList (..))
import qualified GHC.Num                                           as GHC
import           Prelude                                           hiding (Num (..), concat, divMod, length, mod,
                                                                    replicate, splitAt, truncate, (!!), (&&), (^))

import           ZkFold.Base.Algebra.Basic.Class                   (AdditiveGroup (..), AdditiveSemigroup (..),
                                                                    Exponent (..), FromConstant (..),
                                                                    MultiplicativeSemigroup (..), SemiEuclidean (..),
                                                                    divMod, one, zero, (-!))
import           ZkFold.Base.Algebra.Basic.Number
import qualified ZkFold.Base.Data.Vector                           as Vec
import           ZkFold.Prelude                                    (length, replicate, splitAt, (!!))
import           ZkFold.Symbolic.Algorithms.Hash.Blake2b.Constants (blake2b_iv, sigma)
import           ZkFold.Symbolic.Class                             (Symbolic)
import           ZkFold.Symbolic.Data.Bool                         (BoolType (..))
import           ZkFold.Symbolic.Data.ByteString                   (ByteString (..), ShiftBits (..), concat,
                                                                    reverseEndianness, toWords, truncate)
import           ZkFold.Symbolic.Data.Combinators                  (Iso (..), RegisterSize (..))
import           ZkFold.Symbolic.Data.UInt                         (UInt (..))

-- | BLAKE2b Cryptographic hash. Reference:
-- https://tools.ietf.org/html/rfc7693

{-
2.1.  Parameters
   The following table summarizes various parameters and their ranges:
                            | BLAKE2b          | BLAKE2s          |
              --------------+------------------+------------------+
               Bits in word | w = 64           | w = 32           |
               Rounds in F  | r = 12           | r = 10           |
               Block bytes  | bb = 128         | bb = 64          |
               Hash bytes   | 1 <= nn <= 64    | 1 <= nn <= 32    |
               Key bytes    | 0 <= kk <= 64    | 0 <= kk <= 32    |
               Input bytes  | 0 <= ll < 2**128 | 0 <= ll < 2**64  |
              --------------+------------------+------------------+
               G Rotation   | (R1, R2, R3, R4) | (R1, R2, R3, R4) |
                constants = | (32, 24, 16, 63) | (16, 12,  8,  7) |
              --------------+------------------+------------------+
-}

r1 :: Natural
r1 = 32
r2 :: Natural
r2 = 24
r3 :: Natural
r3 = 16
r4 :: Natural
r4 = 63

pow2 :: forall a . FromConstant Natural a => Natural -> a
pow2 = fromConstant @Natural . (2 ^)

shiftUIntR :: forall b . Symbolic b => UInt 64 Auto b -> Natural -> UInt 64 Auto b
shiftUIntR u n = from @_ @(UInt 64 Auto b) $ from @_ @(ByteString 64 b) u `shiftBitsR` n

shiftUIntL :: forall b . Symbolic b => UInt 64 Auto b -> Natural -> UInt 64 Auto b
shiftUIntL u n = u * pow2 n

xorUInt :: forall c . Symbolic c => UInt 64 Auto c ->  UInt 64 Auto c ->  UInt 64 Auto c
xorUInt u1 u2 = from @(ByteString 64 c) @(UInt 64 Auto c) $ from u1 `xor` from u2

-- | state context
data Blake2bCtx c = Blake2bCtx
   { h :: V.Vector (UInt 64 Auto c) -- chained state 8
   , m :: V.Vector (UInt 64 Auto c) -- input buffer 16
   , t :: (Natural, Natural)        -- total number of bytes
   }

-- | Cyclic right rotation.
rotr64 :: Symbolic c => (UInt 64 Auto c, Natural) -> UInt 64 Auto c
rotr64 (x, y) = (x `shiftUIntR` y) `xorUInt` (x `shiftUIntL` (64 -! y))

{-
3.1.  Mixing Function G
   The G primitive function mixes two input words, "x" and "y", into
   four words indexed by "a", "b", "c", and "d" in the working vector
   v[0..15].  The full modified vector is returned.  The rotation
   constants (R1, R2, R3, R4) are given in Section 2.1.
       FUNCTION G( v[0..15], a, b, c, d, x, y )
       |
       |   v[a] := (v[a] + v[b] + x) mod 2**w
       |   v[d] := (v[d] ^ v[a]) >>> R1
       |   v[c] := (v[c] + v[d])     mod 2**w
       |   v[b] := (v[b] ^ v[c]) >>> R2
       |   v[a] := (v[a] + v[b] + y) mod 2**w
       |   v[d] := (v[d] ^ v[a]) >>> R3
       |   v[c] := (v[c] + v[d])     mod 2**w
       |   v[b] := (v[b] ^ v[c]) >>> R4
       |
       |   RETURN v[0..15]
       |
       END FUNCTION.
-}

-- | Little-endian byte access.
b2b_g :: forall c . Symbolic c =>
    V.Vector (UInt 64 Auto c) -> (Int, Int, Int, Int, UInt 64 Auto c, UInt 64 Auto c) -> V.Vector (UInt 64 Auto c)
b2b_g v (a, b, c, d, x, y) =
    let va1 = (v ! a) + (v ! b) + x                 -- v[a] = v[a] + v[b] + x;         \
        vd1 = rotr64 ((v ! d) `xorUInt` va1, r1)    -- v[d] = ROTR64(v[d] ^ v[a], 32); \
        vc1 = (v ! c) + vd1                         -- v[c] = v[c] + v[d];             \
        vb1 = rotr64 ((v ! b) `xorUInt` vc1, r2)    -- v[b] = ROTR64(v[b] ^ v[c], 24); \
        va2 = va1 + vb1 + y                         -- v[a] = v[a] + v[b] + y;         \
        vd2 = rotr64 (vd1 `xorUInt` va2, r3)        -- v[d] = ROTR64(v[d] ^ v[a], 16); \
        vc2 = vc1 + vd2                             -- v[c] = v[c] + v[d];             \
        vb2 = rotr64 (vb1 `xorUInt` vc2, r4)        -- v[b] = ROTR64(v[b] ^ v[c], 63);
    in v // [(a, va2), (b, vb2), (c, vc2), (d, vd2)]

{-
3.2.  Compression Function F
   Compression function F takes as an argument the state vector "h",
   message block vector "m" (last block is padded with zeros to full
   block size, if required), 2w-bit offset counter "t", and final block
   indicator flag "f".  Local vector v[0..15] is used in processing.  F
   returns a new state vector.  The number of rounds, "r", is 12 for
   BLAKE2b and 10 for BLAKE2s.  Rounds are numbered from 0 to r - 1.
       FUNCTION F( h[0..7], m[0..15], t, f )
       |
       |      // Initialize local work vector v[0..15]
       |      v[0..7] := h[0..7]              // First half from state.
       |      v[8..15] := IV[0..7]            // Second half from IV.
       |
       |      v[12] := v[12] ^ (t mod 2**w)   // Low word of the offset.
       |      v[13] := v[13] ^ (t >> w)       // High word.
       |
       |      IF f = TRUE THEN                // last block flag?
       |      |   v[14] := v[14] ^ 0xFF..FF   // Invert all bits.
       |      END IF.
       |
       |      // Cryptographic mixing
       |      FOR i = 0 TO r - 1 DO           // Ten or twelve rounds.
       |      |
       |      |   // Message word selection permutation for this round.
       |      |   s[0..15] := SIGMA[i mod 10][0..15]
       |      |
       |      |   v := G( v, 0, 4,  8, 12, m[s[ 0]], m[s[ 1]] )
       |      |   v := G( v, 1, 5,  9, 13, m[s[ 2]], m[s[ 3]] )
       |      |   v := G( v, 2, 6, 10, 14, m[s[ 4]], m[s[ 5]] )
       |      |   v := G( v, 3, 7, 11, 15, m[s[ 6]], m[s[ 7]] )
       |      |
       |      |   v := G( v, 0, 5, 10, 15, m[s[ 8]], m[s[ 9]] )
       |      |   v := G( v, 1, 6, 11, 12, m[s[10]], m[s[11]] )
       |      |   v := G( v, 2, 7,  8, 13, m[s[12]], m[s[13]] )
       |      |   v := G( v, 3, 4,  9, 14, m[s[14]], m[s[15]] )
       |      |
       |      END FOR
       |
       |      FOR i = 0 TO 7 DO               // XOR the two halves.
       |      |   h[i] := h[i] ^ v[i] ^ v[i + 8]
       |      END FOR.
       |
       |      RETURN h[0..7]                  // New state.
       |
       END FUNCTION.
-}

-- | Compression function. "last" flag indicates the last block.
blake2b_compress :: forall c . Symbolic c => Blake2bCtx c -> Bool -> V.Vector (UInt 64 Auto c)
blake2b_compress Blake2bCtx{h, m, t} lastBlock =
    let v'  = h V.++ blake2b_iv -- init work variables
        v'' = v' V.// [ (12, (v' ! 12) `xorUInt` fromConstant (fst t))  -- low word of the offset
                      , (13, (v' ! 13) `xorUInt` fromConstant (snd t))] -- high word of the offset

        v0 = if lastBlock                                               -- last block flag set ?
                then v'' // [(14, (v'' ! 14) `xorUInt` negate one)]     -- Invert all bits
                else v''

        hashRound w0 i = w8
            where
                s  = sigma ! i
                w1 = b2b_g w0 (0, 4,  8, 12, m ! (s ! 0),  m ! (s ! 1))
                w2 = b2b_g w1 (1, 5,  9, 13, m ! (s ! 2),  m ! (s ! 3))
                w3 = b2b_g w2 (2, 6, 10, 14, m ! (s ! 4),  m ! (s ! 5))
                w4 = b2b_g w3 (3, 7, 11, 15, m ! (s ! 6),  m ! (s ! 7))
                w5 = b2b_g w4 (0, 5, 10, 15, m ! (s ! 8),  m ! (s ! 9))
                w6 = b2b_g w5 (1, 6, 11, 12, m ! (s ! 10), m ! (s ! 11))
                w7 = b2b_g w6 (2, 7,  8, 13, m ! (s ! 12), m ! (s ! 13))
                w8 = b2b_g w7 (3, 4,  9, 14, m ! (s ! 14), m ! (s ! 15))

        v1 = V.foldl hashRound v0 $ fromList [0..11] -- twelve rounds
    in fmap (\(i, hi) -> hi `xorUInt` (v1 ! i) `xorUInt` (v1 ! (i GHC.+ 8))) (V.zip (fromList [0..7]) h)

{-
        FUNCTION BLAKE2( d[0..dd-1], ll, kk, nn )
        |
        |     h[0..7] := IV[0..7]          // Initialization Vector.
        |
        |     // Parameter block p[0]
        |     h[0] := h[0] ^ 0x01010000 ^ (kk << 8) ^ nn
        |
        |     // Process padded key and data blocks
        |     IF dd > 1 THEN
        |     |       FOR i = 0 TO dd - 2 DO
        |     |       |       h := F( h, d[i], (i + 1) * bb, FALSE )
        |     |       END FOR.
        |     END IF.
        |
        |     // Final block.
        |     IF kk = 0 THEN
        |     |       h := F( h, d[dd - 1], ll, TRUE )
        |     ELSE
        |     |       h := F( h, d[dd - 1], ll + bb, TRUE )
        |     END IF.
        |
        |     RETURN first "nn" bytes from little-endian word array h[].
        |
        END FUNCTION.
-}

blake2b' :: forall bb' kk' ll' nn' c .
    ( Symbolic c
    , KnownNat bb'
    , KnownNat kk'
    , KnownNat ll'
    , KnownNat nn'
    ) => [V.Vector (UInt 64 Auto c)] -> ByteString (8 * nn') c
blake2b' d =
    let bb = value @bb'
        ll = value @ll'
        kk = value @kk'
        nn = value @nn'
        dd = bool (ceiling (kk % bb) + ceiling (ll % bb)) 1 (kk == 0 && ll == 0) :: Natural

        toOffset :: forall x . (FromConstant Natural x) => Natural -> (x, x)
        toOffset x = let (hi, lo) = x `divMod` pow2 64 in (fromConstant lo, fromConstant hi)

        h = blake2b_iv :: V.Vector (UInt 64 Auto c)

        -- Parameter block p[0]
        h' = h // [(0, (h ! 0) `xorUInt` fromConstant @Natural 0x01010000 `xorUInt` (fromConstant kk `shiftUIntR` 8) `xorUInt` fromConstant nn)]

        -- Process padded key and data blocks
        h'' = if dd > 1
            then foldl' (\acc i -> blake2b_compress (Blake2bCtx acc (d !! i) (toOffset @Natural $ (i + 1) * bb)) False) h' [0 .. dd -! 2]
            else h'

        -- Final round
        h''' = if kk == 0
            then blake2b_compress (Blake2bCtx h'' (d !! (dd -! 1)) (toOffset @Natural $ ll)) True
            else blake2b_compress (Blake2bCtx h'' (d !! (dd -! 1)) (toOffset @Natural $ ll + bb)) True

        bs = reverseEndianness @64 $ concat @8 @64 $ Vec.unsafeToVector @8 $ map from $ toList h''' :: ByteString (64 * 8) c
    in withDict (timesNLe512 @nn') $ with8n @nn' (truncate bs)

type ExtensionBits inputLen = 8 * (128 - Mod inputLen 128)
type ExtendedInputByteString inputLen = 8 * inputLen + ExtensionBits inputLen

withExtensionBits :: forall n {r}. KnownNat n => (KnownNat (ExtensionBits n) => r) -> r
withExtensionBits = withDict (modBound @n @128) $
                        withDict (modNat @n @128) $
                            withDict (minusNat @128 @(Mod n 128)) $
                                withDict (timesNat @8 @(128 - Mod n 128))

withExtendedInputByteString :: forall n {r}. KnownNat n => (KnownNat (8 * n + ExtensionBits n) => r) -> r
withExtendedInputByteString = withExtensionBits @n $
                                    withDict (timesNat @8 @n) $
                                        withDict (plusNat @(8 * n) @(ExtensionBits n))

with8nLessExt :: forall n {r}. KnownNat n => (8 * n <= 8 * n + ExtensionBits n => r) -> r
with8nLessExt = withExtendedInputByteString @n $
                    withDict (zeroLe @(ExtensionBits n)) $
                        withDict (plusMonotone2 @(8 * n) @0 @(ExtensionBits n))

with8n  :: forall n {r}. KnownNat n => (KnownNat (8 * n) => r) -> r
with8n = withDict (timesNat @8 @n)

timesNLe512 :: forall n . Dict (8 * n <= 512)
timesNLe512 = unsafeAxiom

blake2bDivConstraint :: forall n. Dict (Div (8 * n + ExtensionBits n) 64 * 64 ~ 8 * n + ExtensionBits n)
blake2bDivConstraint = unsafeAxiom

withBlake2bDivConstraint :: forall n {r}. (Div (8 * n + ExtensionBits n) 64 * 64 ~ 8 * n + ExtensionBits n => r) -> r
withBlake2bDivConstraint =  withDict $ blake2bDivConstraint @n

withConstraints :: forall n {r}. KnownNat n => (
    ( KnownNat (8 * n)
    , KnownNat (ExtensionBits n)
    , KnownNat (8 * n + ExtensionBits n)
    , 8 * n <= 8 * n + ExtensionBits n
    , Div (8 * n + ExtensionBits n) 64 * 64 ~ 8 * n + ExtensionBits n) => r) -> r
withConstraints = with8nLessExt @n $ withExtendedInputByteString @n $ withExtensionBits @n $ with8n @n $ withBlake2bDivConstraint @n

nLeInput :: forall n i . Dict (n <= 8 * i)
nLeInput = unsafeAxiom

blake2b :: forall keyLen inputLen outputLen c n.
    ( Symbolic c
    , KnownNat keyLen
    , KnownNat inputLen
    , KnownNat outputLen
    , n ~ (8 * inputLen + ExtensionBits inputLen)
    ) => Natural -> ByteString (8 * inputLen) c -> ByteString (8 * outputLen) c
blake2b key input =
    let input' = withConstraints @inputLen $
                    withDict (nLeInput @n @inputLen) $
                    from <$> (toWords @(Div n 64) @64 $
                    reverseEndianness @64 $
                    flip rotateBitsL (value @(ExtensionBits inputLen)) $
                    truncate @_ @(ExtendedInputByteString inputLen) input ) :: Vec.Vector (Div n 64) (UInt 64 Auto c)

        key'    = fromConstant @_ key :: UInt 64 Auto c
        input'' = if value @keyLen > 0
            then key' : Vec.fromVector input'
            else Vec.fromVector input'

        padding = length input'' `mod` 16
        input''' = input'' ++ replicate (16 -! padding) zero

        toBlocks :: [w] -> [V.Vector w]
        toBlocks [] = []
        toBlocks xs = let (a, b) = splitAt 16 xs in fromList a : toBlocks b

        d = toBlocks input'''
    in blake2b'
        @128       -- block size bb'
        @keyLen    -- key length kk'
        @inputLen  -- input length ll'
        d

{-
            Algorithm     | Target | Collision | Hash | Hash ASN.1 |
               Identifier |  Arch  |  Security |  nn  | OID Suffix |
           ---------------+--------+-----------+------+------------+
            id-blake2b160 | 64-bit |   2**80   |  20  |   x.1.5    |
            id-blake2b256 | 64-bit |   2**128  |  32  |   x.1.8    |
            id-blake2b384 | 64-bit |   2**192  |  48  |   x.1.12   |
            id-blake2b512 | 64-bit |   2**256  |  64  |   x.1.16   |
           ---------------+--------+-----------+------+------------+
-}

-- | Hash a `ByteString` using the Blake2b-224 hash function.
blake2b_224 :: forall inputLen c.
    ( Symbolic c
    , KnownNat inputLen
    ) => ByteString (8 * inputLen) c -> ByteString 224 c
blake2b_224 = blake2b @0 @inputLen @28 0

-- | Hash a `ByteString` using the Blake2b-256 hash function.
blake2b_256 :: forall inputLen c.
    ( Symbolic c
    , KnownNat inputLen
    ) => ByteString (8 * inputLen) c -> ByteString 256 c
blake2b_256 = blake2b @0 @inputLen @32 0

-- | Hash a `ByteString` using the Blake2b-512 hash function.
blake2b_512 :: forall inputLen c.
    ( Symbolic c
    , KnownNat inputLen
    ) => ByteString (8 * inputLen) c -> ByteString 512 c
blake2b_512 = blake2b @0 @inputLen @64 0
