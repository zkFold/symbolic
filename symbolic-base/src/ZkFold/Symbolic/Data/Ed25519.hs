{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.Ed25519 (Ed25519_Point) where

import           Prelude                                   (fromInteger, type (~), ($))
import qualified Prelude                                   as P

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.EllipticCurve.Ed25519 (Ed25519_Base, Ed25519_PointOf)
import           ZkFold.Symbolic.Class                     (Symbolic (..))
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators          (RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.FFA
import           ZkFold.Symbolic.Data.FieldElement

type Ed25519_Point ctx = Ed25519_PointOf (FFA Ed25519_Base 'Auto ctx)

instance (Symbolic ctx, KnownFFA Ed25519_Base 'Auto ctx) => CyclicGroup (Ed25519_Point ctx) where
  type ScalarFieldOf (Ed25519_Point ctx) = FieldElement ctx
  pointGen = pointXY
    (fromConstant (15112221349535400772501151409588531511454012693041857206046113283949847762202 :: Natural))
    (fromConstant (46316835694926478169428394003475163141307993866256225615783033603165251855960 :: Natural))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , bits ~ NumberOfBits a
  , KnownFFA Ed25519_Base 'Auto ctx
  ) => Scale (FieldElement ctx) (Ed25519_Point ctx) where

    scale sc x = sum $ P.zipWith (\b p -> bool @(Bool ctx) zero p (isSet bits b)) [upper, upper -! 1 .. 0] (P.iterate (\e -> e + e) x)
        where
            bits :: ByteString bits ctx
            bits = ByteString $ binaryExpansion sc

            upper :: Natural
            upper = value @bits -! 1
