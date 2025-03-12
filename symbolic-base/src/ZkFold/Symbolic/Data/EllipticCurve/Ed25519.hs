{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.Ed25519 (Ed25519_Point) where

import           Prelude                                   (fromInteger, type (~), ($))
import qualified Prelude                                   as P

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.EllipticCurve.Ed25519 (Ed25519_Base, Ed25519_Scalar, Ed25519_PointOf)
import           ZkFold.Symbolic.Class                     (Symbolic (..))
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators          (RegisterSize (Auto), from)
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.FFA

type Ed25519_Point ctx = Ed25519_PointOf (FFA Ed25519_Base 'Auto ctx)

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , nativeBits ~ NumberOfBits a
  , uintBits ~ FFAUIntSize Ed25519_Scalar (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA Ed25519_Base 'Auto ctx
  , KnownFFA Ed25519_Scalar 'Auto ctx
  ) => CyclicGroup (Ed25519_Point ctx) where
  type ScalarFieldOf (Ed25519_Point ctx) = FFA Ed25519_Scalar 'Auto ctx
  pointGen = pointXY
    (fromConstant (15112221349535400772501151409588531511454012693041857206046113283949847762202 :: Natural))
    (fromConstant (46316835694926478169428394003475163141307993866256225615783033603165251855960 :: Natural))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , nativeBits ~ NumberOfBits a
  , uintBits ~ FFAUIntSize Ed25519_Scalar (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA Ed25519_Base 'Auto ctx
  , KnownFFA Ed25519_Scalar 'Auto ctx
  ) => Scale (FFA Ed25519_Scalar 'Auto ctx) (Ed25519_Point ctx) where

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
