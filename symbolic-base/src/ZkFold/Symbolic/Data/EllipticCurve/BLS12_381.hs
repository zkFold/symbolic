{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.BLS12_381 (BLS12_381_G1_Point) where

import Prelude (($))
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Base, BLS12_381_Scalar)
import qualified ZkFold.Algebra.EllipticCurve.BLS12_381 as Haskell
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Number
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.FFA

type BLS12_381_G1_Point ctx = Weierstrass "BLS12-381-G1" (Point (FFA BLS12_381_Base 'Auto ctx))

instance
  (Symbolic ctx, KnownFFA BLS12_381_Base Auto ctx)
  => FromConstant Haskell.BLS12_381_G1_Point (BLS12_381_G1_Point ctx)
  where
  fromConstant (Weierstrass (Point x y b)) =
    Weierstrass (Point (fromConstant x) (fromConstant y) (fromConstant b))

instance
  ( Symbolic ctx
  , KnownFFA BLS12_381_Base 'Auto ctx
  , KnownFFA BLS12_381_Scalar 'Auto ctx
  )
  => CyclicGroup (BLS12_381_G1_Point ctx)
  where
  type ScalarFieldOf (BLS12_381_G1_Point ctx) = FFA BLS12_381_Scalar 'Auto ctx
  pointGen =
    pointXY
      ( fromConstant
          (0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb :: Natural)
      )
      ( fromConstant
          (0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1 :: Natural)
      )

instance
  ( Symbolic ctx
  , KnownFFA BLS12_381_Base 'Auto ctx
  , KnownFFA BLS12_381_Scalar 'Auto ctx
  , FromConstant k (FFA BLS12_381_Scalar 'Auto ctx)
  )
  => Scale k (BLS12_381_G1_Point ctx)
  where
  scale (fromConstant -> (ffa :: FFA BLS12_381_Scalar 'Auto ctx)) x =
    sum $
      Prelude.zipWith
        (\b p -> bool zero p (isSet bits b))
        [upper, upper -! 1 .. 0]
        (Prelude.iterate (\e -> e + e) x)
   where
    bits :: ByteString (FFAMaxBits BLS12_381_Scalar ctx) ctx
    bits = from (toUInt @(FFAMaxBits BLS12_381_Scalar ctx) ffa)

    upper :: Natural
    upper = value @(FFAMaxBits BLS12_381_Scalar ctx) -! 1
