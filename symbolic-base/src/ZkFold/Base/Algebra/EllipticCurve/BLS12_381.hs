{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Base.Algebra.EllipticCurve.BLS12_381 where

import           Control.Monad
import           Data.Bits
import           Data.Foldable
import           Data.Word
import           Prelude                                    hiding (Num (..), (/), (^))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.EllipticCurve.Pairing
import           ZkFold.Base.Algebra.Polynomials.Univariate
import           ZkFold.Base.Data.ByteString
import qualified ZkFold.Symbolic.Data.Conditional           as Symbolic
import qualified ZkFold.Symbolic.Data.Eq                    as Symbolic

-------------------------------- Introducing Fields ----------------------------------

type BLS12_381_Scalar = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
instance Prime BLS12_381_Scalar

type BLS12_381_Base = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
instance Prime BLS12_381_Base

type Fr = Zp BLS12_381_Scalar
type Fq = Zp BLS12_381_Base

type IP1 = "IP1"
instance IrreduciblePoly Fq IP1 where
    irreduciblePoly = toPoly [1, 0, 1]
type Fq2 = Ext2 Fq IP1

type IP2 = "IP2"
instance IrreduciblePoly Fq2 IP2 where
    irreduciblePoly =
        let e = Ext2
                0xd0088f51cbff34d258dd3db21a5d66bb23ba5c279c2895fb39869507b587b120f55ffff58a9ffffdcff7fffffffd556
                0xd0088f51cbff34d258dd3db21a5d66bb23ba5c279c2895fb39869507b587b120f55ffff58a9ffffdcff7fffffffd555
        in toPoly [negate e, zero, zero, one]
type Fq6 = Ext3 Fq2 IP2

type IP3 = "IP3"
instance IrreduciblePoly Fq6 IP3 where
    irreduciblePoly =
        let e = Ext3 zero (negate one) zero
        in toPoly [e, zero, one]
type Fq12 = Ext2 Fq6 IP3

------------------------------------- BLS12-381-G1 --------------------------------------

instance Field field => WeierstrassCurve "BLS12-381-G1" field where
  weierstrassB = fromConstant (4 :: Natural)

type BLS12_381_G1_Point = Weierstrass "BLS12-381-G1" (Point Fq)

type BLS12_381_G1_CompressedPoint =
  Weierstrass "BLS12-381-G1" (CompressedPoint Fq)

instance Compressible BLS12_381_G1_Point where
    type Compressed BLS12_381_G1_Point = BLS12_381_G1_CompressedPoint
    pointCompressed x yBit = Weierstrass (CompressedPoint x yBit False)
    compress (Weierstrass (Point x y isInf)) =
      if isInf then pointInf
      else pointCompressed @BLS12_381_G1_Point x (y > negate y)
    decompress (Weierstrass (CompressedPoint x bigY isInf)) =
      if isInf then pointInf else
        let b = weierstrassB @"BLS12-381-G1"
            q = order @Fq
            sqrt_ z = z ^ ((q + 1) `Prelude.div` 2)
            y' = sqrt_ (x * x * x + b)
            y'' = negate y'
            y = if bigY then max y' y'' else min y' y''
        in  pointXY x y

instance CyclicGroup BLS12_381_G1_Point where
  type ScalarFieldOf BLS12_381_G1_Point = Fr
  pointGen = pointXY
    0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb
    0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1

instance Scale Fr BLS12_381_G1_Point where
  scale n x = scale (toConstant n) x

------------------------------------ BLS12-381 G2 ------------------------------------

instance WeierstrassCurve "BLS12-381-G2" Fq2 where
  weierstrassB = Ext2 4 4

type BLS12_381_G2_Point = Weierstrass "BLS12-381-G2" (Point Fq2)

type BLS12_381_G2_CompressedPoint =
  Weierstrass "BLS12-381-G2" (CompressedPoint Fq2)

instance CyclicGroup BLS12_381_G2_Point where
  type ScalarFieldOf BLS12_381_G2_Point = Fr
  pointGen = pointXY
    (Ext2
      0x24aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8
      0x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e)
    (Ext2
      0xce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801
      0x606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be)

instance Scale Fr BLS12_381_G2_Point where
  scale n x = scale (toConstant n) x

instance Compressible BLS12_381_G2_Point where
    type Compressed BLS12_381_G2_Point = BLS12_381_G2_CompressedPoint
    pointCompressed x yBit = Weierstrass (CompressedPoint x yBit False)
    compress (Weierstrass (Point x y isInf)) =
      if isInf then pointInf
      else pointCompressed @BLS12_381_G2_Point x (y > negate y)
    decompress (Weierstrass (CompressedPoint x bigY isInf)) =
      if isInf then pointInf else
        let b = weierstrassB @"BLS12-381-G2"
            q = order @Fq2
            sqrt_ z = z ^ ((q + 1) `Prelude.div` 2)
            y' = sqrt_ (x * x * x + b)
            y'' = negate y'
            y = if bigY then max y' y'' else min y' y''
        in  pointXY x y

------------------------------------ Encoding ------------------------------------

-- infinite list of divMod 256's, little endian order
leBytesOf :: Natural -> [(Natural, Word8)]
leBytesOf n =
    let
        (n', r) = n `Prelude.divMod` 256
    in
        (n', fromIntegral r) : leBytesOf n'

-- finite list of bytes, big endian order
bytesOf :: (ToConstant a, Const a ~ Natural) => Int -> a -> [Word8]
bytesOf n
    = reverse
    . take n
    . map snd
    . leBytesOf
    . toConstant

-- big endian decoding
ofBytes :: FromConstant Natural a => [Word8] -> a
ofBytes
  = fromConstant @Natural
  . foldl' (\n w8 -> n * 256 + fromIntegral w8) 0

instance Binary BLS12_381_G1_Point where
    put (Weierstrass (Point x y isInf)) =
        if isInf then foldMap putWord8 (bitReverse8 (bit 1) : replicate 95 0)
                 else foldMap putWord8 (bytesOf 48 x <> bytesOf 48 y)
    get = do
        byte <- bitReverse8 <$> getWord8
        let compressed = testBit byte 0
            infinite = testBit byte 1
        if infinite then do
            skip (if compressed then 47 else 95)
            return pointInf
        else do
            let byteXhead = bitReverse8 $ clearBit (clearBit (clearBit byte 0) 1) 2
            bytesXtail <- replicateM 47 getWord8
            let x = ofBytes (byteXhead:bytesXtail)
                bigY = testBit byte 2
            if compressed then return $
              decompress @BLS12_381_G1_Point
                (pointCompressed @BLS12_381_G1_Point x bigY)
            else do
                bytesY <- replicateM 48 getWord8
                let y = ofBytes bytesY
                return (pointXY x y)

instance Binary BLS12_381_G1_CompressedPoint where
    put (Weierstrass (CompressedPoint x bigY isInf)) =
        if isInf then foldMap putWord8 (bitReverse8 (bit 0 .|. bit 1) : replicate 47 0) else
        let
            flags = bitReverse8 $ if bigY then bit 0 .|. bit 2 else bit 0
            bytes = bytesOf 48 x
        in foldMap putWord8 ((flags .|. head bytes) : tail bytes)
    get = do
        byte <- bitReverse8 <$> getWord8
        let compressed = testBit byte 0
            infinite = testBit byte 1
        if infinite then do
            skip (if compressed then 47 else 95)
            return pointInf
        else do
            let byteXhead = bitReverse8 $ clearBit (clearBit (clearBit byte 0) 1) 2
            bytesXtail <- replicateM 47 getWord8
            let x = ofBytes (byteXhead:bytesXtail)
                bigY = testBit byte 2
            pointCompressed @BLS12_381_G1_Point x <$>
              if compressed then return bigY else do
                bytesY <- replicateM 48 getWord8
                let y :: Fq = ofBytes bytesY
                    bigY' = y > negate y
                return bigY'

instance Binary BLS12_381_G2_Point where
    put (Weierstrass (Point (Ext2 x0 x1) (Ext2 y0 y1) isInf)) =
        if isInf then foldMap putWord8 (bitReverse8 (bit 1) : replicate 191  0) else
        let
            bytes = bytesOf 48 x1
              <> bytesOf 48 x0
              <> bytesOf 48 y1
              <> bytesOf 48 y0
        in
            foldMap putWord8 bytes
    get = do
        byte <- bitReverse8 <$> getWord8
        let compressed = testBit byte 0
            infinite = testBit byte 1
        if infinite then do
            skip (if compressed then 95 else 191)
            return pointInf
        else do
            let byteX1head = bitReverse8 $ clearBit (clearBit (clearBit byte 0) 1) 2
            bytesX1tail <- replicateM 47 getWord8
            bytesX0 <- replicateM 48 getWord8
            let x1 = ofBytes (byteX1head:bytesX1tail)
                x0 = ofBytes bytesX0
                bigY = testBit byte 2
            if compressed then return $
              decompress @BLS12_381_G2_Point
                (pointCompressed @BLS12_381_G2_Point (Ext2 x0 x1) bigY)
            else do
                bytesY1 <- replicateM 48 getWord8
                bytesY0 <- replicateM 48 getWord8
                let y0 = ofBytes bytesY0
                    y1 = ofBytes bytesY1
                return (pointXY (Ext2 x0 x1) (Ext2 y0 y1))

instance Binary BLS12_381_G2_CompressedPoint where
    put (Weierstrass (CompressedPoint (Ext2 x0 x1) bigY isInf)) =
        if isInf then foldMap putWord8 (bitReverse8 (bit 0 .|. bit 1) : replicate 95 0) else
        let
            flags = bitReverse8 $ if bigY then bit 0 .|. bit 2 else bit 0
            bytes = bytesOf 48 x1 <> bytesOf 48 x0
        in
            foldMap putWord8 ((flags .|. head bytes) : tail bytes)
    get = do
        byte <- bitReverse8 <$> getWord8
        let compressed = testBit byte 0
            infinite = testBit byte 1
        if infinite then do
            skip (if compressed then 95 else 191)
            return pointInf
        else do
            let byteX1head = bitReverse8 $ clearBit (clearBit (clearBit byte 0) 1) 2
            bytesX1tail <- replicateM 47 getWord8
            bytesX0 <- replicateM 48 getWord8
            let x1 = ofBytes (byteX1head:bytesX1tail)
                x0 = ofBytes bytesX0
                x = Ext2 x0 x1
                bigY = testBit byte 2
            pointCompressed @BLS12_381_G2_Point x <$>
              if compressed then return bigY else do
                bytesY1 <- replicateM 48 getWord8
                bytesY0 <- replicateM 48 getWord8
                let y0 = ofBytes bytesY0
                    y1 = ofBytes bytesY1
                    y :: Fq2 = Ext2 y0 y1
                    bigY' = y > negate y
                return bigY'

--------------------------------------- Pairing ---------------------------------------

-- | An image of a pairing is a cyclic multiplicative subgroup of @'Fq12'@
-- of order @'BLS12_381_Scalar'@.
newtype BLS12_381_GT = BLS12_381_GT Fq12
    deriving newtype
        ( Eq
        , Show
        , MultiplicativeSemigroup
        , MultiplicativeMonoid
        , Symbolic.Conditional Prelude.Bool
        , Symbolic.Eq
        )

instance Exponent BLS12_381_GT Natural where
    BLS12_381_GT a ^ p = BLS12_381_GT (a ^ p)

instance Exponent BLS12_381_GT Integer where
    BLS12_381_GT a ^ p = BLS12_381_GT (a ^ p)

deriving via (NonZero Fq12) instance MultiplicativeGroup BLS12_381_GT

instance Finite BLS12_381_GT where
    type Order BLS12_381_GT = BLS12_381_Scalar

instance Pairing BLS12_381_G1_Point BLS12_381_G2_Point BLS12_381_GT where
    pairing a b
      = BLS12_381_GT
      $ finalExponentiation @Fr
      $ millerAlgorithmBLS12 param a b
      where
        param = [-1
          ,-1, 0,-1, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0
          , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
          , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0
          , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
          ]
