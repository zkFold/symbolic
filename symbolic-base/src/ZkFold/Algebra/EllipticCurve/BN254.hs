{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Algebra.EllipticCurve.BN254 (
  BN254_Scalar,
  BN254_Base,
  Fr,
  Fp,
  Fp2,
  Fp6,
  Fp12,
  BN254_G1_Point,
  BN254_G2_Point,
  BN254_G1_JacobianPoint,
  BN254_G2_JacobianPoint,
  BN254_GT,
) where

import Control.Monad (return, (>>))
import Data.Binary (Binary (..))
import Data.Bool ((&&))
import Data.Function (($))
import Text.Show (Show)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.EllipticCurve.Pairing
import ZkFold.Algebra.Field (Ext2 (..), Ext3 (..), IrreduciblePoly (..), Zp)
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Univariate (Poly, toPoly)
import ZkFold.Symbolic.Data.Conditional
import ZkFold.Symbolic.Data.Eq
import Prelude (Bool, Integer)
import qualified Prelude

-------------------------- Scalar field & field towers -------------------------

-- Designations of curve parameters are as in:
-- https://pkg.go.dev/github.com/consensys/gnark-crypto/ecc/bn254

type BN254_Scalar = 21888242871839275222246405745257275088548364400416034343698204186575808495617

instance Prime BN254_Scalar

type BN254_Base = 21888242871839275222246405745257275088696311157297823662689037894645226208583

instance Prime BN254_Base

type Fr = Zp BN254_Scalar

type Fp = Zp BN254_Base

instance IrreduciblePoly (Poly Fp) Fp "BN254-IP1" where
  irreduciblePoly = toPoly @Fp @(Poly Fp) [1, 0, 1]

type Fp2 = Ext2 Fp "BN254-IP1"

-- cubic nonresidue in @Fp2@.
xi :: Fp2
xi = Ext2 9 1

instance IrreduciblePoly (Poly Fp2) Fp2 "BN254-IP2" where
  irreduciblePoly = toPoly @Fp2 @(Poly Fp2) [negate xi, zero, zero, one]

type Fp6 = Ext3 Fp2 "BN254-IP2"

instance IrreduciblePoly (Poly Fp6) Fp6 "BN254-IP3" where
  irreduciblePoly = toPoly @Fp6 @(Poly Fp6) [Ext3 zero (negate one) zero, zero, one]

type Fp12 = Ext2 Fp6 "BN254-IP3"

------------------------------- bn254 G1 ---------------------------------------

type BN254_G1_Point = BN254_G1_PointOf Fp

type BN254_G1_JacobianPoint = BN254_G1_JacobianPointOf Fp

type BN254_G1_PointOf field = Weierstrass "BN254_G1" (Point field)

type BN254_G1_JacobianPointOf field = Weierstrass "BN254_G1" (JacobianPoint field)

instance Field field => WeierstrassCurve "BN254_G1" field where
  weierstrassB = fromConstant (3 :: Natural)

instance CyclicGroup BN254_G1_Point where
  type ScalarFieldOf BN254_G1_Point = Fr
  pointGen = pointXY 1 2

instance Scale Fr BN254_G1_Point where
  scale n x = scale (toConstant n) x

instance CyclicGroup BN254_G1_JacobianPoint where
  type ScalarFieldOf BN254_G1_JacobianPoint = Fr
  pointGen = project @BN254_G1_Point pointGen

instance Scale Fr BN254_G1_JacobianPoint where
  scale n x = scale (toConstant n) x

------------------------------- bn254 G2 ---------------------------------------

type BN254_G2_Point = BN254_G2_PointOf Fp2

type BN254_G2_JacobianPoint = BN254_G2_JacobianPointOf Fp2

type BN254_G2_PointOf field = Weierstrass "BN254_G2" (Point field)

type BN254_G2_JacobianPointOf field = Weierstrass "BN254_G2" (JacobianPoint field)

instance WeierstrassCurve "BN254_G2" Fp2 where
  weierstrassB =
    Ext2
      0x2b149d40ceb8aaae81be18991be06ac3b5b4c5e559dbefa33267e6dc24a138e5
      0x9713b03af0fed4cd2cafadeed8fdf4a74fa084e52d1852e4a2bd0685c315d2

instance CyclicGroup BN254_G2_Point where
  type ScalarFieldOf BN254_G2_Point = Fr
  pointGen =
    pointXY
      ( Ext2
          0x1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed
          0x198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2
      )
      ( Ext2
          0x12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa
          0x90689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b
      )

instance Scale Fr BN254_G2_Point where
  scale n x = scale (toConstant n) x

instance CyclicGroup BN254_G2_JacobianPoint where
  type ScalarFieldOf BN254_G2_JacobianPoint = Fr
  pointGen = project @BN254_G2_Point pointGen

instance Scale Fr BN254_G2_JacobianPoint where
  scale n x = scale (toConstant n) x

------------------------------- Pairing ----------------------------------------

newtype BN254_GT = BN254_GT Fp12
  deriving
    ( Conditional Prelude.Bool
    , Eq
    , MultiplicativeMonoid
    , MultiplicativeSemigroup
    , Prelude.Eq
    , Show
    )

instance Exponent BN254_GT Natural where
  BN254_GT e ^ p = BN254_GT (e ^ p)

instance Exponent BN254_GT Integer where
  BN254_GT e ^ p = BN254_GT (e ^ p)

deriving via (NonZero Fp12) instance MultiplicativeGroup BN254_GT

instance Finite BN254_GT where
  type Order BN254_GT = BN254_Scalar

instance Pairing BN254_G1_Point BN254_G2_Point BN254_GT where
  pairing p q =
    BN254_GT $
      finalExponentiation @Fr $
        millerAlgorithmBN xi param p q
   where
{- FOURMOLU_DISABLE -}
    param = [ 1
      , 1, 0, 1, 0, 0,-1, 0, 1, 1, 0, 0, 0,-1, 0, 0, 1
      , 1, 0, 0,-1, 0, 0, 0, 0, 0, 1, 0, 0,-1, 0, 0, 1
      , 1, 1, 0, 0, 0, 0,-1, 0, 1, 0, 0,-1, 0, 1, 1, 0
      , 0, 1, 0, 0,-1, 1, 0, 0,-1, 0, 1, 0, 1, 0, 0, 0
      ]
{- FOURMOLU_ENABLE -}

------------------------------ Encoding ----------------------------------------

instance Binary BN254_G1_Point where
  put (Weierstrass (Point xp yp isInf)) =
    if isInf then put @BN254_G1_Point (pointXY zero zero) else put xp >> put yp
  get = do
    xp <- get
    yp <- get
    return $
      if xp == zero && yp == zero
        then pointInf
        else pointXY xp yp

instance Binary BN254_G2_Point where
  put (Weierstrass (Point xp yp isInf)) =
    if isInf then put @BN254_G2_Point (pointXY zero zero) else put xp >> put yp
  get = do
    xp <- get
    yp <- get
    return $
      if xp == zero && yp == zero
        then pointInf
        else pointXY xp yp
