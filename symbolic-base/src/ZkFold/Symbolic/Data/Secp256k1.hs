{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.Secp256k1 (Secp256k1_Point) where

import           Prelude                                     (fromInteger, type (~), ($))
import qualified Prelude                                     as P

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.EllipticCurve.Secp256k1 (Secp256k1_Base, Secp256k1_PointOf)
import           ZkFold.Symbolic.Class                       (Symbolic (..))
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators            (RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.FFA
import           ZkFold.Symbolic.Data.FieldElement

type Secp256k1_Point ctx = Secp256k1_PointOf (FFA Secp256k1_Base 'Auto ctx)

instance (Symbolic ctx, KnownFFA Secp256k1_Base 'Auto ctx) => CyclicGroup (Secp256k1_Point ctx) where
  type ScalarFieldOf (Secp256k1_Point ctx) = FieldElement ctx
  pointGen = pointXY
    (fromConstant (0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798 :: Natural))
    (fromConstant (0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8 :: Natural))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , bits ~ NumberOfBits a
  , KnownFFA Secp256k1_Base 'Auto ctx
  ) => Scale (FieldElement ctx) (Secp256k1_Point ctx) where

    scale sc x = sum $ P.zipWith (\b p -> bool @(Bool ctx) zero p (isSet bits b)) [upper, upper -! 1 .. 0] (P.iterate (\e -> e + e) x)
        where
            bits :: ByteString bits ctx
            bits = ByteString $ binaryExpansion sc

            upper :: Natural
            upper = value @bits -! 1
