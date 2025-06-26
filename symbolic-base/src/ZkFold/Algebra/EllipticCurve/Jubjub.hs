{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Algebra.EllipticCurve.Jubjub (
  Jubjub_Base,
  Jubjub_Scalar,
  Jubjub_Point,
  Jubjub_PointOf,
  Fl,
  Fq,
) where

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Field
import ZkFold.Algebra.Number

-- | As defined in https://github.com/zkcrypto/jubjub.
type Jubjub_Scalar = 6554484396890773809930967563523245729705921265872317281365359162392183254199

instance Prime Jubjub_Scalar

-- | "The choice of GF(q) is made to be the scalar field of the BLS12-381 elliptic curve
-- construction."
type Jubjub_Base = BLS12_381_Scalar

type Jubjub_PointOf baseField = TwistedEdwards "jubjub" (AffinePoint baseField)

type Jubjub_Point = Jubjub_PointOf Fq

type Fl = Zp Jubjub_Scalar

type Fq = Zp Jubjub_Base

instance Field field => TwistedEdwardsCurve "jubjub" field where
  twistedEdwardsA = negate one
  twistedEdwardsD =
    negate fromConstant (10240 :: Natural)
      // fromConstant (10241 :: Natural)

instance CyclicGroup Jubjub_Point where
  type ScalarFieldOf Jubjub_Point = Fl
  pointGen =
    pointXY
      (fromConstant (8076246640662884909881801758704306714034609987455869804520522091855516602923 :: Natural))
      (fromConstant (13262374693698910701929044844600465831413122818447359594527400194675274060458 :: Natural))

instance Scale Fl Jubjub_Point where
  scale n = scale (toConstant n)
