{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Use camelCase" -}

module ZkFold.Symbolic.Data.EllipticCurve.PlutoEris (Pluto_Point, Eris_Point) where

import Prelude (fromInteger, ($))
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class hiding (Point)
import ZkFold.Algebra.EllipticCurve.PlutoEris (PlutoEris_p, PlutoEris_q)
import ZkFold.Algebra.Number
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Data.EllipticCurve.Point (Point)
import ZkFold.Symbolic.Data.FFA
import ZkFold.Symbolic.Data.UInt (uintToBSbe)

type Pluto_Point = Point (Weierstrass "Pluto-Eris") (FFA PlutoEris_p)

type Eris_Point = Point (Weierstrass "Pluto-Eris") (FFA PlutoEris_q)

instance
  ( Symbolic ctx
  , KnownFFA PlutoEris_p ctx
  , KnownFFA PlutoEris_q ctx
  )
  => CyclicGroup (Pluto_Point ctx)
  where
  type ScalarFieldOf (Pluto_Point ctx) = FFA PlutoEris_q ctx
  pointGen =
    pointXY
      (fromConstant (-2 :: Prelude.Integer))
      (fromConstant (7 :: Prelude.Integer))

instance
  ( Symbolic ctx
  , KnownFFA PlutoEris_p ctx
  , KnownFFA PlutoEris_q ctx
  )
  => Scale (FFA PlutoEris_q ctx) (Pluto_Point ctx)
  where
  scale ffa x =
    sum $
      Prelude.zipWith
        (\b p -> bool zero p $ isSet bits b)
        [upper, upper -! 1 .. 0]
        (Prelude.iterate (\e -> e + e) x)
   where
    bits :: ByteString (FFAMaxBits PlutoEris_q ctx) ctx
    bits = uintToBSbe $ toUInt @(FFAMaxBits PlutoEris_q ctx) ffa

    upper :: Natural
    upper = value @(FFAMaxBits PlutoEris_q ctx) -! 1

instance
  ( Symbolic ctx
  , KnownFFA PlutoEris_p ctx
  , KnownFFA PlutoEris_q ctx
  )
  => CyclicGroup (Eris_Point ctx)
  where
  type ScalarFieldOf (Eris_Point ctx) = FFA PlutoEris_p ctx
  pointGen =
    pointXY
      (fromConstant (-2 :: Prelude.Integer))
      (fromConstant (7 :: Prelude.Integer))

instance
  ( Symbolic ctx
  , KnownFFA PlutoEris_p ctx
  , KnownFFA PlutoEris_q ctx
  )
  => Scale (FFA PlutoEris_p ctx) (Eris_Point ctx)
  where
  scale ffa x =
    sum $
      Prelude.zipWith
        (\b p -> bool zero p $ isSet bits b)
        [upper, upper -! 1 .. 0]
        (Prelude.iterate (\e -> e + e) x)
   where
    bits :: ByteString (FFAMaxBits PlutoEris_p ctx) ctx
    bits = uintToBSbe $ toUInt @(FFAMaxBits PlutoEris_p ctx) ffa

    upper :: Natural
    upper = value @(FFAMaxBits PlutoEris_p ctx) -! 1
