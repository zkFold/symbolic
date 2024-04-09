{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Base.Algebra.EllipticCurve.BLS12_381 where

import Data.Bits (shiftR)
import Data.List (unfoldr)
import Numeric.Natural (Natural)
import ZkFold.Base.Algebra.Basic.Class
import ZkFold.Base.Algebra.Basic.Field
import ZkFold.Base.Algebra.Basic.Number
import ZkFold.Base.Algebra.EllipticCurve.Class
import ZkFold.Base.Algebra.Polynomials.Univariate
import Prelude hiding (Num (..), (/), (^))

-------------------------------- Introducing Fields ----------------------------------

type BLS12_381_Scalar = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

instance Prime BLS12_381_Scalar

type BLS12_381_Base = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab

instance Prime BLS12_381_Base

type Fr = Zp BLS12_381_Scalar

type Fq = Zp BLS12_381_Base

data IP1

instance IrreduciblePoly Fq IP1 where
  irreduciblePoly = toPoly [1, 0, 1]

type Fq2 = Ext2 Fq IP1

data IP2

instance IrreduciblePoly Fq2 IP2 where
  irreduciblePoly =
    let e =
          Ext2
            0xd0088f51cbff34d258dd3db21a5d66bb23ba5c279c2895fb39869507b587b120f55ffff58a9ffffdcff7fffffffd556
            0xd0088f51cbff34d258dd3db21a5d66bb23ba5c279c2895fb39869507b587b120f55ffff58a9ffffdcff7fffffffd555
     in toPoly [negate e, zero, zero, one]

type Fq6 = Ext3 Fq2 IP2

data IP3

instance IrreduciblePoly Fq6 IP3 where
  irreduciblePoly =
    let e = Ext3 zero (negate one) zero
     in toPoly [e, zero, one]

type Fq12 = Ext2 Fq6 IP3

------------------------------------ BLS12-381 G1 ------------------------------------

data BLS12_381_G1

type instance ScalarField BLS12_381_G1 = Fr

type instance BaseField BLS12_381_G1 = Fq

instance EllipticCurve BLS12_381_G1 where
  inf = Inf

  gen =
    Point
      0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb
      0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1

  add = addPoints

  mul = pointMul

------------------------------------ BLS12-381 G2 ------------------------------------

data BLS12_381_G2

type instance ScalarField BLS12_381_G2 = Fr

type instance BaseField BLS12_381_G2 = Fq2

instance EllipticCurve BLS12_381_G2 where
  inf = Inf

  gen =
    Point
      ( Ext2
          0x24aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8
          0x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e
      )
      ( Ext2
          0xce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801
          0x606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be
      )

  add = addPoints

  mul = pointMul

--------------------------------------- Pairing ---------------------------------------

type BLS12_381_GT = Fq12

instance Pairing BLS12_381_G1 BLS12_381_G2 BLS12_381_GT where
  pairing = pairingBLS

-- Adapted from https://github.com/nccgroup/pairing-bls12381/blob/master/Crypto/Pairing_bls12381.hs

-- Untwist point on E2 for pairing calculation
untwist :: Point BLS12_381_G2 -> (BLS12_381_GT, BLS12_381_GT)
untwist (Point x1 y1) = (wideX, wideY)
  where
    root = Ext3 zero one zero
    wideX = Ext2 zero (Ext3 zero zero x1) / Ext2 zero root
    wideY = Ext2 zero (Ext3 zero zero y1) / Ext2 root zero
untwist Inf = error "untwist: point at infinity"

-- Used in miller loop for computing line functions l_r,r and v_2r
doubleEval :: Point BLS12_381_G2 -> Point BLS12_381_G1 -> BLS12_381_GT
doubleEval r (Point px py) = fromConstant py - (fromConstant px * slope) - v
  where
    (rx, ry) = untwist r
    slope = (rx * rx + rx * rx + rx * rx) / (ry + ry)
    v = ry - slope * rx
doubleEval _ Inf = error "doubleEval: point at infinity"

-- Used in miller loop for computer line function l_r,p and v_r+p
addEval :: Point BLS12_381_G2 -> Point BLS12_381_G2 -> Point BLS12_381_G1 -> BLS12_381_GT
addEval r q p@(Point px _) =
  if (rx == qx) && (ry + qy == zero)
    then fromConstant px - rx
    else addEval' (rx, ry) (qx, qy) p
  where
    (rx, ry) = untwist r
    (qx, qy) = untwist q
addEval _ _ Inf = error "addEval: point at infinity"

-- Helper function for addEval
addEval' :: (BLS12_381_GT, BLS12_381_GT) -> (BLS12_381_GT, BLS12_381_GT) -> Point BLS12_381_G1 -> BLS12_381_GT
addEval' (rx, ry) (qx, qy) (Point px py) = fromConstant py - (fromConstant px * slope) - v
  where
    slope = (qy - ry) / (qx - rx)
    v = ((qy * rx) - (ry * qx)) / (rx - qx)
addEval' _ _ Inf = error "addEval': point at infinity"

-- Classic Miller loop for Ate pairing
miller :: Point BLS12_381_G1 -> Point BLS12_381_G2 -> BLS12_381_GT
miller p q = miller' p q q iterations one
  where
    iterations =
      tail $
        reverse $ -- list of true/false per bits of operand
          unfoldr
            ( \b ->
                if b == (0 :: Integer)
                  then Nothing
                  else Just (odd b, shiftR b 1)
            )
            0xd201000000010000

-- Double and add loop helper for Miller (iterative)
miller' :: Point BLS12_381_G1 -> Point BLS12_381_G2 -> Point BLS12_381_G2 -> [Bool] -> BLS12_381_GT -> BLS12_381_GT
miller' _ _ _ [] result = result
miller' p q r (i : iters) result =
  if i
    then miller' p q (pointAdd doubleR q) iters (accum * addEval doubleR q p)
    else miller' p q doubleR iters accum
  where
    accum = result * result * doubleEval r p
    doubleR = pointDouble r

-- | Pairing calculation for a valid point in G1 and another valid point in G2.
pairingBLS :: Point BLS12_381_G1 -> Point BLS12_381_G2 -> BLS12_381_GT
pairingBLS Inf _ = zero
pairingBLS _ Inf = zero
pairingBLS p q = pow' (miller p q) (((order @(BaseField BLS12_381_G1)) ^ (12 :: Integer) - 1) `div` (order @(ScalarField BLS12_381_G1))) one

-- Used for the final exponentiation; opportunity for further perf optimization
pow' :: (Field a) => a -> Natural -> a -> a
pow' a0 e result
  | e <= 1 = a0
  | even e = accum2
  | otherwise = accum2 * a0
  where
    accum = pow' a0 (shiftR e 1) result
    accum2 = accum * accum
