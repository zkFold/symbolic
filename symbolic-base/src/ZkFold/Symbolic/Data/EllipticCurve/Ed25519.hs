{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.Ed25519 (Ed25519_Point) where

import Data.Function (($))
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class hiding (AffinePoint)
import ZkFold.Algebra.EllipticCurve.Ed25519 (Ed25519_Base, Ed25519_Scalar)
import ZkFold.Algebra.Number
import ZkFold.Algebra.Field (Zp)
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto), from)
import ZkFold.Symbolic.Data.EllipticCurve.Point.Affine (AffinePoint (..))
import ZkFold.Symbolic.Data.FFA

type Ed25519_Point = AffinePoint (TwistedEdwards "ed25519") (FFA Ed25519_Base 'Auto)

instance
  ( Symbolic ctx
  , KnownFFA Ed25519_Base 'Auto ctx
  , KnownFFA Ed25519_Scalar 'Auto ctx
  )
  => CyclicGroup (Ed25519_Point ctx)
  where
  type ScalarFieldOf (Ed25519_Point ctx) = FFA Ed25519_Scalar 'Auto ctx
  pointGen =
    pointXY
      (fromConstant (15112221349535400772501151409588531511454012693041857206046113283949847762202 :: Natural))
      (fromConstant (46316835694926478169428394003475163141307993866256225615783033603165251855960 :: Natural))

instance
  ( Symbolic ctx
  , KnownFFA Ed25519_Base 'Auto ctx
  , KnownFFA Ed25519_Scalar 'Auto ctx
  )
  => Scale (FFA Ed25519_Scalar 'Auto ctx) (Ed25519_Point ctx)
  where
  scale ffa x =
    sum $
      Prelude.zipWith
        (\i p -> bool zero p (isSet bits (upper -! i)))
        [0 .. upper]
        (Prelude.iterate (\e -> e + e) x)
   where
    bits :: ByteString (NumberOfBits (Zp Ed25519_Scalar)) ctx
    bits = from (toUInt @(NumberOfBits (Zp Ed25519_Scalar)) ffa)

    upper :: Natural
    upper = value @(NumberOfBits (Zp Ed25519_Scalar)) -! 1
