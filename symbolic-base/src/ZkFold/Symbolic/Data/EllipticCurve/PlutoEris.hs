{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.PlutoEris (Pluto_Point, Eris_Point) where

import           Prelude                                     (fromInteger, ($))
import qualified Prelude

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.EllipticCurve.PlutoEris (PlutoEris_p, PlutoEris_q)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.FFA

type Pluto_Point ctx =
  Weierstrass "Pluto-Eris" (Point (FFA PlutoEris_p 'Auto ctx))

type Eris_Point ctx =
  Weierstrass "Pluto-Eris" (Point (FFA PlutoEris_q 'Auto ctx))

instance
  ( Symbolic ctx
  , KnownFFA PlutoEris_p 'Auto ctx
  , KnownFFA PlutoEris_q 'Auto ctx
  ) => CyclicGroup (Pluto_Point ctx) where
  type ScalarFieldOf (Pluto_Point ctx) = FFA PlutoEris_q 'Auto ctx
  pointGen = pointXY
    (fromConstant (-2 :: Prelude.Integer))
    (fromConstant (7 :: Prelude.Integer))

instance
  ( Symbolic ctx
  , KnownFFA PlutoEris_p 'Auto ctx
  , KnownFFA PlutoEris_q 'Auto ctx
  ) => Scale (FFA PlutoEris_q 'Auto ctx) (Pluto_Point ctx) where

    scale ffa x = sum $ Prelude.zipWith
      (\b p -> bool @(Bool ctx) zero p (isSet bits b))
      [upper, upper -! 1 .. 0]
      (Prelude.iterate (\e -> e + e) x)
        where
          bits :: ByteString (FFAMaxBits PlutoEris_q ctx) ctx
          bits = from (toUInt @(FFAMaxBits PlutoEris_q ctx) ffa)

          upper :: Natural
          upper = value @(FFAMaxBits PlutoEris_q ctx) -! 1

instance
  ( Symbolic ctx
  , KnownFFA PlutoEris_p 'Auto ctx
  , KnownFFA PlutoEris_q 'Auto ctx
  ) => CyclicGroup (Eris_Point ctx) where
  type ScalarFieldOf (Eris_Point ctx) = FFA PlutoEris_p 'Auto ctx
  pointGen = pointXY
    (fromConstant (-2 :: Prelude.Integer))
    (fromConstant (7 :: Prelude.Integer))

instance
  ( Symbolic ctx
  , KnownFFA PlutoEris_p 'Auto ctx
  , KnownFFA PlutoEris_q 'Auto ctx
  ) => Scale (FFA PlutoEris_p 'Auto ctx) (Eris_Point ctx) where

    scale ffa x = sum $ Prelude.zipWith
      (\b p -> bool @(Bool ctx) zero p (isSet bits b))
      [upper, upper -! 1 .. 0]
      (Prelude.iterate (\e -> e + e) x)
        where
          bits :: ByteString (FFAMaxBits PlutoEris_p ctx) ctx
          bits = from (toUInt @(FFAMaxBits PlutoEris_p ctx) ffa)

          upper :: Natural
          upper = value @(FFAMaxBits PlutoEris_p ctx) -! 1
