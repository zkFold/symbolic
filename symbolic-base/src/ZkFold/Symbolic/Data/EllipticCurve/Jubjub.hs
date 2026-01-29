{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Use camelCase" -}

module ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point) where

import Data.Function (($))
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class hiding (AffinePoint)
import ZkFold.Algebra.EllipticCurve.Jubjub (Jubjub_Base, Jubjub_Scalar)
import ZkFold.Algebra.Number
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Data.EllipticCurve.Point.Affine (AffinePoint (..))
import ZkFold.Symbolic.Data.FFA
import ZkFold.Symbolic.Data.UInt (uintToBSbe)

type Jubjub_Point = AffinePoint (TwistedEdwards "jubjub") (FFA Jubjub_Base)

instance
  ( Symbolic ctx
  , KnownFFA Jubjub_Base ctx
  , KnownFFA Jubjub_Scalar ctx
  )
  => CyclicGroup (Jubjub_Point ctx)
  where
  type ScalarFieldOf (Jubjub_Point ctx) = FFA Jubjub_Scalar ctx
  pointGen =
    pointXY
      (fromConstant (8076246640662884909881801758704306714034609987455869804520522091855516602923 :: Natural))
      (fromConstant (13262374693698910701929044844600465831413122818447359594527400194675274060458 :: Natural))

instance
  ( Symbolic ctx
  , KnownFFA Jubjub_Base ctx
  , KnownFFA Jubjub_Scalar ctx
  )
  => Scale (FFA Jubjub_Scalar ctx) (Jubjub_Point ctx)
  where
  scale ffa x =
    sum $
      Prelude.zipWith
        (\b p -> bool zero p $ isSet bits b)
        [upper, upper -! 1 .. 0]
        (Prelude.iterate (\e -> e + e) x)
   where
    bits :: ByteString (FFAMaxBits Jubjub_Scalar ctx) ctx
    bits = uintToBSbe $ toUInt @(FFAMaxBits Jubjub_Scalar ctx) ffa

    upper :: Natural
    upper = value @(FFAMaxBits Jubjub_Scalar ctx) -! 1
