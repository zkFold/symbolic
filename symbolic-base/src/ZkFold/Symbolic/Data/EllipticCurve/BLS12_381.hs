{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.BLS12_381 (BLS12_381_G1_Point) where

import           Prelude                                     (fromInteger, type (~), ($))
import qualified Prelude

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Base, BLS12_381_Scalar)
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Symbolic.Class                       (Symbolic (..))
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators            (RegisterSize (Auto), from)
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.FFA

type BLS12_381_G1_Point ctx = Weierstrass "BLS12-381-G1" (Point (FFA BLS12_381_Base 'Auto ctx))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , nativeBits ~ NumberOfBits a
  , uintBits ~ FFAUIntSize BLS12_381_Scalar (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA BLS12_381_Base 'Auto ctx
  , KnownFFA BLS12_381_Scalar 'Auto ctx
  ) => CyclicGroup (BLS12_381_G1_Point ctx) where
  type ScalarFieldOf (BLS12_381_G1_Point ctx) = FFA BLS12_381_Scalar 'Auto ctx
  pointGen = pointXY
    (fromConstant (0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb :: Natural))
    (fromConstant (0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1 :: Natural))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , nativeBits ~ NumberOfBits a
  , uintBits ~ FFAUIntSize BLS12_381_Scalar (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA BLS12_381_Base 'Auto ctx
  , KnownFFA BLS12_381_Scalar 'Auto ctx
  ) => Scale (FFA BLS12_381_Scalar 'Auto ctx) (BLS12_381_G1_Point ctx) where

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
