{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.Secp256k1 (Secp256k1_Point) where

import           Prelude                                     (fromInteger, ($))
import qualified Prelude

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.EllipticCurve.Secp256k1 (Secp256k1_Base, Secp256k1_PointOf, Secp256k1_Scalar)
import           ZkFold.Symbolic.Class                       (Symbolic (..))
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators            (RegisterSize (Auto), from)
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.FFA

type Secp256k1_Point ctx = Secp256k1_PointOf (FFA Secp256k1_Base 'Auto ctx)

instance
  ( Symbolic ctx
  , KnownFFA Secp256k1_Base 'Auto ctx
  , KnownFFA Secp256k1_Scalar 'Auto ctx
  ) => CyclicGroup (Secp256k1_Point ctx) where
  type ScalarFieldOf (Secp256k1_Point ctx) = FFA Secp256k1_Scalar 'Auto ctx
  pointGen = pointXY
    (fromConstant (0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798 :: Natural))
    (fromConstant (0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8 :: Natural))

instance
  ( Symbolic ctx
  , KnownFFA Secp256k1_Base 'Auto ctx
  , KnownFFA Secp256k1_Scalar 'Auto ctx
  ) => Scale (FFA Secp256k1_Scalar 'Auto ctx) (Secp256k1_Point ctx) where

    scale ffa x = sum $ Prelude.zipWith
      (\b p -> bool @(Bool ctx) zero p (isSet bits b))
      [upper, upper -! 1 .. 0]
      (Prelude.iterate (\e -> e + e) x)
        where
          bits :: ByteString (FFAMaxBits Secp256k1_Scalar ctx) ctx
          bits = from (toUInt @(FFAMaxBits Secp256k1_Scalar ctx) ffa)

          upper :: Natural
          upper = value @(FFAMaxBits Secp256k1_Scalar ctx) -! 1
