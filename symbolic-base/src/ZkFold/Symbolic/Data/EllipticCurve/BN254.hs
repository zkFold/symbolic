{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.BN254 (BN254_G1_Point) where

import           Prelude                                 (fromInteger, type (~), ($))
import qualified Prelude

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.BN254 (BN254_Base, BN254_Scalar)
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Symbolic.Class                   (Symbolic (..))
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators        (RegisterSize (Auto), from)
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.FFA

type BN254_G1_Point ctx = Weierstrass "BN254_G1" (Point (FFA BN254_Base 'Auto ctx))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , nativeBits ~ NumberOfBits a
  , uintBits ~ FFAUIntSize BN254_Scalar (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA BN254_Base 'Auto ctx
  , KnownFFA BN254_Scalar 'Auto ctx
  ) => CyclicGroup (BN254_G1_Point ctx) where
  type ScalarFieldOf (BN254_G1_Point ctx) = FFA BN254_Scalar 'Auto ctx
  pointGen = pointXY
    (fromConstant (1 :: Natural))
    (fromConstant (2 :: Natural))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , nativeBits ~ NumberOfBits a
  , uintBits ~ FFAUIntSize BN254_Scalar (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA BN254_Base 'Auto ctx
  , KnownFFA BN254_Scalar 'Auto ctx
  ) => Scale (FFA BN254_Scalar 'Auto ctx) (BN254_G1_Point ctx) where

    scale (FFA nativeSc uintSc) x = sum $ Prelude.zipWith
      (\b p -> bool @(Bool ctx) zero p (isSet (nativeBits `append` uintBits) b))
      [upper, upper -! 1 .. 0]
      (Prelude.iterate (\e -> e + e) x)
        where
            nativeBits :: ByteString nativeBits ctx
            nativeBits = ByteString $ binaryExpansion nativeSc

            uintBits :: ByteString uintBits ctx
            uintBits = from uintSc

            upper :: Natural
            upper = value @(nativeBits + uintBits) -! 1
