{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Algebra.EllipticCurve.Secp256k1 (
  Secp256k1_Base,
  Secp256k1_Scalar,
  Secp256k1_Point,
  Secp256k1_PointOf,
  Secp256k1_JacobianPoint,
  Secp256k1_JacobianPointOf,
  Fn,
  Fp,
) where

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Field
import ZkFold.Algebra.Number

type Secp256k1_Scalar = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

instance Prime Secp256k1_Scalar

type Secp256k1_Base = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F

instance Prime Secp256k1_Base

type Secp256k1_Point = Secp256k1_PointOf Fp

type Secp256k1_PointOf field = Weierstrass "secp256k1" (Point field)

type Secp256k1_JacobianPoint = Secp256k1_JacobianPointOf Fp

type Secp256k1_JacobianPointOf field = Weierstrass "secp256k1" (JacobianPoint field)

type Fn = Zp Secp256k1_Scalar

type Fp = Zp Secp256k1_Base

instance Field field => WeierstrassCurve "secp256k1" field where
  weierstrassB = fromConstant (7 :: Natural)

instance CyclicGroup Secp256k1_Point where
  type ScalarFieldOf Secp256k1_Point = Fn
  pointGen =
    pointXY
      0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
      0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8

instance Scale Fn Secp256k1_Point where
  scale n x = scale (toConstant n) x

instance CyclicGroup Secp256k1_JacobianPoint where
  type ScalarFieldOf Secp256k1_JacobianPoint = Fn
  pointGen = project @Secp256k1_Point pointGen

instance Scale Fn Secp256k1_JacobianPoint where
  scale n x = scale (toConstant n) x
