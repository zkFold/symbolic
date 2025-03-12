{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.Secp256k1 (Secp256k1_Point) where

import           Prelude                                     (fromInteger, type (~), ($))
import qualified Prelude                                     as P

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.EllipticCurve.Secp256k1 (Secp256k1_Base, Secp256k1_Scalar, Secp256k1_PointOf)
import           ZkFold.Symbolic.Class                       (Symbolic (..))
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators            (from, RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.FFA

type Secp256k1_Point ctx = Secp256k1_PointOf (FFA Secp256k1_Base 'Auto ctx)

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , nativeBits ~ NumberOfBits a
  , uintBits ~ FFAUIntSize Secp256k1_Scalar (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA Secp256k1_Base 'Auto ctx
  , KnownFFA Secp256k1_Scalar 'Auto ctx
  ) => CyclicGroup (Secp256k1_Point ctx) where
  type ScalarFieldOf (Secp256k1_Point ctx) = FFA Secp256k1_Scalar 'Auto ctx
  pointGen = pointXY
    (fromConstant (0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798 :: Natural))
    (fromConstant (0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8 :: Natural))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , nativeBits ~ NumberOfBits a
  , uintBits ~ FFAUIntSize Secp256k1_Scalar (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA Secp256k1_Base 'Auto ctx
  , KnownFFA Secp256k1_Scalar 'Auto ctx
  ) => Scale (FFA Secp256k1_Scalar 'Auto ctx) (Secp256k1_Point ctx) where

    scale (FFA nativeSc uintSc) x = sum $ P.zipWith
      (\b p -> bool @(Bool ctx) zero p (isSet (nativeBits `append` uintBits) b))
      [upper, upper -! 1 .. 0]
      (P.iterate (\e -> e + e) x)
        where
            nativeBits :: ByteString nativeBits ctx
            nativeBits = ByteString $ binaryExpansion nativeSc 

            uintBits :: ByteString uintBits ctx
            uintBits = from uintSc

            upper :: Natural
            upper = value @(nativeBits + uintBits) -! 1
