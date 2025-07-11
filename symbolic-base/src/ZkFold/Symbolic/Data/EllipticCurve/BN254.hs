{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.BN254 (BN254_G1_Point) where

import Data.Function (($))
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BN254 (BN254_Base, BN254_Scalar)
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Number
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto), from)
import ZkFold.Symbolic.Data.FFA

type BN254_G1_Point ctx = Weierstrass "BN254_G1" (Point (FFA BN254_Base 'Auto ctx))

instance
  ( Symbolic ctx
  , KnownFFA BN254_Base 'Auto ctx
  , KnownFFA BN254_Scalar 'Auto ctx
  )
  => CyclicGroup (BN254_G1_Point ctx)
  where
  type ScalarFieldOf (BN254_G1_Point ctx) = FFA BN254_Scalar 'Auto ctx
  pointGen =
    pointXY
      (fromConstant (1 :: Natural))
      (fromConstant (2 :: Natural))

instance
  ( Symbolic ctx
  , KnownFFA BN254_Base 'Auto ctx
  , KnownFFA BN254_Scalar 'Auto ctx
  )
  => Scale (FFA BN254_Scalar 'Auto ctx) (BN254_G1_Point ctx)
  where
  scale ffa x =
    sum $
      Prelude.zipWith
        (\b p -> bool zero p (isSet bits b))
        [upper, upper -! 1 .. 0]
        (Prelude.iterate (\e -> e + e) x)
   where
    bits :: ByteString (FFAMaxBits BN254_Scalar ctx) ctx
    bits = from (toUInt @(FFAMaxBits BN254_Scalar ctx) ffa)

    upper :: Natural
    upper = value @(FFAMaxBits BN254_Scalar ctx) -! 1
