{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Base.Algebra.EllipticCurve.Ed25519
  ( Ed25519_Base
  , Ed25519_Scalar
  , Ed25519_Point
  , Ed25519_PointOf
  , Fl
  , Fq
  ) where

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.Class

-- | 2^252 + 27742317777372353535851937790883648493 is the order of the multiplicative group in Ed25519
-- with the generator point defined below in @instance EllipticCurve (Ed25519 Void r)@
--
type Ed25519_Scalar = 7237005577332262213973186563042994240857116359379907606001950938285454250989
instance Prime Ed25519_Scalar

-- | 2^255 - 19 is the order of the base field from which point coordinates are taken.
--
type Ed25519_Base = 57896044618658097711785492504343953926634992332820282019728792003956564819949
instance Prime Ed25519_Base

type Ed25519_PointOf baseField = TwistedEdwards "ed25519" (AffinePoint baseField)
type Ed25519_Point = Ed25519_PointOf Fq

type Fl = Zp Ed25519_Scalar
type Fq = Zp Ed25519_Base

instance Field field => TwistedEdwardsCurve "ed25519" field where
  twistedEdwardsA = negate one
  twistedEdwardsD =
    negate fromConstant (121665 :: Natural)
        // fromConstant (121666 :: Natural)

instance CyclicGroup Ed25519_Point where
  type ScalarFieldOf Ed25519_Point = Fl
  pointGen = pointXY
    (fromConstant (15112221349535400772501151409588531511454012693041857206046113283949847762202 :: Natural))
    (fromConstant (46316835694926478169428394003475163141307993866256225615783033603165251855960 :: Natural))

instance Scale Fl Ed25519_Point where
  scale n = scale (toConstant n)
