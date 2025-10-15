{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.Pasta (Pallas_Point, Vesta_Point) where

import Prelude (fromInteger, ($))
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class hiding (Point)
import ZkFold.Algebra.EllipticCurve.Pasta (FpModulus, FqModulus)
import ZkFold.Algebra.Number
import ZkFold.Algebra.Field (Zp)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.EllipticCurve.Point (Point)
import ZkFold.Symbolic.Data.FFA

type Pallas_Point = Point (Weierstrass "Pasta") (FFA FpModulus 'Auto)

type Vesta_Point = Point (Weierstrass "Pasta") (FFA FqModulus 'Auto)

instance
  ( Symbolic ctx
  , KnownFFA FpModulus 'Auto ctx
  , KnownFFA FqModulus 'Auto ctx
  )
  => CyclicGroup (Pallas_Point ctx)
  where
  type ScalarFieldOf (Pallas_Point ctx) = FFA FqModulus 'Auto ctx
  pointGen =
    pointXY
      (fromConstant (0x40000000000000000000000000000000224698fc094cf91b992d30ed00000000 :: Natural))
      (fromConstant (0x02 :: Natural))

instance
  ( Symbolic ctx
  , KnownFFA FpModulus 'Auto ctx
  , KnownFFA FqModulus 'Auto ctx
  )
  => Scale (FFA FqModulus 'Auto ctx) (Pallas_Point ctx)
  where
  scale ffa x =
    sum $
      Prelude.zipWith
        (\i p -> bool zero p (isSet bits (upper -! i)))
        [0 .. upper]
        (Prelude.iterate (\e -> e + e) x)
   where
    bits :: ByteString (NumberOfBits (Zp FqModulus)) ctx
    bits = from (toUInt @(NumberOfBits (Zp FqModulus)) ffa)

    upper :: Natural
    upper = value @(NumberOfBits (Zp FqModulus)) -! 1

instance
  ( Symbolic ctx
  , KnownFFA FpModulus 'Auto ctx
  , KnownFFA FqModulus 'Auto ctx
  )
  => CyclicGroup (Vesta_Point ctx)
  where
  type ScalarFieldOf (Vesta_Point ctx) = FFA FpModulus 'Auto ctx
  pointGen =
    pointXY
      (fromConstant (0x40000000000000000000000000000000224698fc094cf91b992d30ed00000000 :: Natural))
      (fromConstant (0x02 :: Natural))

instance
  ( Symbolic ctx
  , KnownFFA FpModulus 'Auto ctx
  , KnownFFA FqModulus 'Auto ctx
  )
  => Scale (FFA FpModulus 'Auto ctx) (Vesta_Point ctx)
  where
  scale ffa x =
    sum $
      Prelude.zipWith
        (\i p -> bool zero p (isSet bits (upper -! i)))
        [0 .. upper]
        (Prelude.iterate (\e -> e + e) x)
   where
    bits :: ByteString (NumberOfBits (Zp FpModulus)) ctx
    bits = from (toUInt @(NumberOfBits (Zp FpModulus)) ffa)

    upper :: Natural
    upper = value @(NumberOfBits (Zp FpModulus)) -! 1
