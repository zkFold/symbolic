{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Use camelCase" -}

module ZkFold.Symbolic.Data.EllipticCurve.Pasta (Pallas_Point, Vesta_Point) where

import Prelude (fromInteger, ($))
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class hiding (Point)
import ZkFold.Algebra.EllipticCurve.Pasta (FpModulus, FqModulus)
import ZkFold.Algebra.Number
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Data.EllipticCurve.Point (Point)
import ZkFold.Symbolic.Data.FFA
import ZkFold.Symbolic.Data.UInt (uintToBSbe)

type Pallas_Point = Point (Weierstrass "Pasta") (FFA FpModulus)

type Vesta_Point = Point (Weierstrass "Pasta") (FFA FqModulus)

instance
  ( Symbolic ctx
  , KnownFFA FpModulus ctx
  , KnownFFA FqModulus ctx
  )
  => CyclicGroup (Pallas_Point ctx)
  where
  type ScalarFieldOf (Pallas_Point ctx) = FFA FqModulus ctx
  pointGen =
    pointXY
      (fromConstant (0x40000000000000000000000000000000224698fc094cf91b992d30ed00000000 :: Natural))
      (fromConstant (0x02 :: Natural))

instance
  ( Symbolic ctx
  , KnownFFA FpModulus ctx
  , KnownFFA FqModulus ctx
  )
  => Scale (FFA FqModulus ctx) (Pallas_Point ctx)
  where
  scale ffa x =
    sum $
      Prelude.zipWith
        (\b p -> bool zero p $ isSet bits b)
        [upper, upper -! 1 .. 0]
        (Prelude.iterate (\e -> e + e) x)
   where
    bits :: ByteString (FFAMaxBits FqModulus ctx) ctx
    bits = uintToBSbe $ toUInt @(FFAMaxBits FqModulus ctx) ffa

    upper :: Natural
    upper = value @(FFAMaxBits FqModulus ctx) -! 1

instance
  ( Symbolic ctx
  , KnownFFA FpModulus ctx
  , KnownFFA FqModulus ctx
  )
  => CyclicGroup (Vesta_Point ctx)
  where
  type ScalarFieldOf (Vesta_Point ctx) = FFA FpModulus ctx
  pointGen =
    pointXY
      (fromConstant (0x40000000000000000000000000000000224698fc094cf91b992d30ed00000000 :: Natural))
      (fromConstant (0x02 :: Natural))

instance
  ( Symbolic ctx
  , KnownFFA FpModulus ctx
  , KnownFFA FqModulus ctx
  )
  => Scale (FFA FpModulus ctx) (Vesta_Point ctx)
  where
  scale ffa x =
    sum $
      Prelude.zipWith
        (\b p -> bool zero p $ isSet bits b)
        [upper, upper -! 1 .. 0]
        (Prelude.iterate (\e -> e + e) x)
   where
    bits :: ByteString (FFAMaxBits FpModulus ctx) ctx
    bits = uintToBSbe $ toUInt @(FFAMaxBits FpModulus ctx) ffa

    upper :: Natural
    upper = value @(FFAMaxBits FpModulus ctx) -! 1
