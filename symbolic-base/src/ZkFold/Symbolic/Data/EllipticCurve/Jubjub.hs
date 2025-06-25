{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point) where

import           Data.Function                        (($))
import qualified Prelude

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.Class
import           ZkFold.Algebra.EllipticCurve.Jubjub (Jubjub_Base, Jubjub_PointOf, Jubjub_Scalar)
import           ZkFold.Algebra.Number
import           ZkFold.Symbolic.Class                (Symbolic (..))
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators     (RegisterSize (Auto), from)
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.FFA

type Jubjub_Point ctx = Jubjub_PointOf (FFA Jubjub_Base 'Auto ctx)

instance
  ( Symbolic ctx
  , KnownFFA Jubjub_Base 'Auto ctx
  , KnownFFA Jubjub_Scalar 'Auto ctx
  ) => CyclicGroup (Jubjub_Point ctx) where
  type ScalarFieldOf (Jubjub_Point ctx) = FFA Jubjub_Scalar 'Auto ctx
  pointGen = pointXY
    (fromConstant (8076246640662884909881801758704306714034609987455869804520522091855516602923 :: Natural))
    (fromConstant (13262374693698910701929044844600465831413122818447359594527400194675274060458 :: Natural))

instance
  ( Symbolic ctx
  , KnownFFA Jubjub_Base 'Auto ctx
  , KnownFFA Jubjub_Scalar 'Auto ctx
  ) => Scale (FFA Jubjub_Scalar 'Auto ctx) (Jubjub_Point ctx) where

    scale ffa x = sum $ Prelude.zipWith
      (\b p -> bool @(Bool ctx) zero p (isSet bits b))
      [upper, upper -! 1 .. 0]
      (Prelude.iterate (\e -> e + e) x)
        where
          bits :: ByteString (FFAMaxBits Jubjub_Scalar ctx) ctx
          bits = from (toUInt @(FFAMaxBits Jubjub_Scalar ctx) ffa)

          upper :: Natural
          upper = value @(FFAMaxBits Jubjub_Scalar ctx) -! 1
