{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Algebra.EllipticCurve.PlutoEris (
  PlutoEris_p,
  PlutoEris_q,
  Pluto_Point,
  Eris_Point,
  Triton_Point,
  Pluto_JacobianPoint,
  Eris_JacobianPoint,
  Triton_JacobianPoint,
) where

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Field
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Univariate (Poly, toPoly)

-- Designations of curve parameters are as in:
-- https://github.com/daira/pluto-eris

-------------------------- Pluto/Eris curves -------------------------

type PlutoEris_p =
  0x24000000000024000130e0000d7f70e4a803ca76f439266f443f9a5cda8a6c7be4a7a5fe8fadffd6a2a7e8c30006b9459ffffcd300000001

instance Prime PlutoEris_p

type PlutoEris_q =
  0x24000000000024000130e0000d7f70e4a803ca76f439266f443f9a5c7a8a6c7be4a775fe8e177fd69ca7e85d60050af41ffffcd300000001

instance Prime PlutoEris_q

instance Field field => WeierstrassCurve "Pluto-Eris" field where
  weierstrassB = fromConstant (57 :: Natural)

type Pluto_Point = Weierstrass "Pluto-Eris" (Point (Zp PlutoEris_p))

instance CyclicGroup Pluto_Point where
  type ScalarFieldOf Pluto_Point = Zp PlutoEris_q
  pointGen = pointXY (-2) 7

instance Scale (Zp PlutoEris_q) Pluto_Point where
  scale n = scale (toConstant n)

type Pluto_JacobianPoint = Weierstrass "Pluto-Eris" (JacobianPoint (Zp PlutoEris_p))

instance CyclicGroup Pluto_JacobianPoint where
  type ScalarFieldOf Pluto_JacobianPoint = Zp PlutoEris_q
  pointGen = project @Pluto_Point pointGen

instance Scale (Zp PlutoEris_q) Pluto_JacobianPoint where
  scale n = scale (toConstant n)

type Eris_Point = Weierstrass "Pluto-Eris" (Point (Zp PlutoEris_q))

instance CyclicGroup Eris_Point where
  type ScalarFieldOf Eris_Point = Zp PlutoEris_p
  pointGen = pointXY (-2) 7

instance Scale (Zp PlutoEris_p) Eris_Point where
  scale n = scale (toConstant n)

type Eris_JacobianPoint = Weierstrass "Pluto-Eris" (JacobianPoint (Zp PlutoEris_q))

instance CyclicGroup Eris_JacobianPoint where
  type ScalarFieldOf Eris_JacobianPoint = Zp PlutoEris_p
  pointGen = project @Eris_Point pointGen

instance Scale (Zp PlutoEris_p) Eris_JacobianPoint where
  scale n = scale (toConstant n)

-------------------------- Triton curve ----------------------------

-- The definition of Triton has not been finalized and is subject to change

instance IrreduciblePoly (Poly (Zp PlutoEris_p)) (Zp PlutoEris_p) "i*sqrt5" where
  irreduciblePoly = toPoly [5, 0, 1]

instance WeierstrassCurve "Triton" (Ext2 (Zp PlutoEris_p) "i*sqrt5") where
  weierstrassB = Ext2 3 1

type Triton_Point =
  Weierstrass "Triton" (Point (Ext2 (Zp PlutoEris_p) "i*sqrt5"))

type Triton_JacobianPoint =
  Weierstrass "Triton" (JacobianPoint (Ext2 (Zp PlutoEris_p) "i*sqrt5"))
