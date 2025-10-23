{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.Secp256k1 (Secp256k1_Point) where

import Data.Function (($))
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class hiding (Point)
import ZkFold.Algebra.EllipticCurve.Secp256k1 (Secp256k1_Base, Secp256k1_Scalar)
import ZkFold.Algebra.Number
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Data.EllipticCurve.Point (Point)
import ZkFold.Symbolic.Data.FFA
import ZkFold.Symbolic.Data.UInt (uintToBSbe)

type Secp256k1_Point = Point (Weierstrass "secp256k1") (FFA Secp256k1_Base)

instance
  ( Symbolic ctx
  , KnownFFA Secp256k1_Base ctx
  , KnownFFA Secp256k1_Scalar ctx
  )
  => CyclicGroup (Secp256k1_Point ctx)
  where
  type ScalarFieldOf (Secp256k1_Point ctx) = FFA Secp256k1_Scalar ctx
  pointGen =
    pointXY
      (fromConstant (0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798 :: Natural))
      (fromConstant (0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8 :: Natural))

instance
  ( Symbolic ctx
  , KnownFFA Secp256k1_Base ctx
  , KnownFFA Secp256k1_Scalar ctx
  )
  => Scale (FFA Secp256k1_Scalar ctx) (Secp256k1_Point ctx)
  where
  scale ffa x =
    sum $
      Prelude.zipWith
        (\b p -> bool zero p $ isSet bits b)
        [upper, upper -! 1 .. 0]
        (Prelude.iterate (\e -> e + e) x)
   where
    bits :: ByteString (FFAMaxBits Secp256k1_Scalar ctx) ctx
    bits = uintToBSbe $ toUInt @(FFAMaxBits Secp256k1_Scalar ctx) ffa

    upper :: Natural
    upper = value @(FFAMaxBits Secp256k1_Scalar ctx) -! 1
