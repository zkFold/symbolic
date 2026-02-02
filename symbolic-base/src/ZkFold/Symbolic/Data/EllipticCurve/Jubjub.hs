{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point, jubjubAdd) where

import Data.Function (($))
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class hiding (AffinePoint)
import qualified ZkFold.Algebra.EllipticCurve.Class as EC
import ZkFold.Algebra.EllipticCurve.Jubjub (Jubjub_Base, Jubjub_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Bool (Bool (..))
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto))
import ZkFold.Symbolic.Data.EllipticCurve.Point.Affine (AffinePoint (..))
import ZkFold.Symbolic.Data.FFA
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))

type Jubjub_Point = AffinePoint (TwistedEdwards "jubjub") (FFA Jubjub_Base 'Auto)

-- | Jubjub curve parameter d = -10240/10241 (mod BLS12_381_Scalar)
jubjubD :: Natural
jubjubD = 19257038036680949359750312669786877991949435402254120286184196891950884077233

-- | Point addition on the Jubjub curve using TwistedEdwards addition formula:
--   x3 = (x0*y1 + y0*x1) / (1 + d*x0*x1*y0*y1)
--   y3 = (y0*y1 + x0*x1) / (1 - d*x0*x1*y0*y1)  [with a = -1]
--
-- Assumes inputs are valid curve points (denominators are non-zero).
jubjubAdd
  :: forall ctx
   . ( Symbolic ctx
     , KnownFFA Jubjub_Base 'Auto ctx
     )
  => Jubjub_Point ctx
  -> Jubjub_Point ctx
  -> Jubjub_Point ctx
jubjubAdd (AffinePoint (EC.AffinePoint x0 y0)) (AffinePoint (EC.AffinePoint x1 y1)) =
  let -- Common subexpressions
      t1 = x0 * x1
      t2 = y0 * y1
      t3 = x0 * y1
      t4 = y0 * x1
      t5 = t1 * t2
      
      -- Numerators
      numX = t3 + t4
      numY = t2 + t1  -- For a = -1: y0*y1 - a*x0*x1 = y0*y1 + x0*x1
      
      -- Inverse of affine denominators: 1/(1 ± d*t5)
      invDenX = ffaInvAffineOrFail (1 :: Natural) jubjubD t5
      invDenY = ffaInvAffineOrFail (1 :: Natural) negJubjubD t5
      
      -- Result coordinates
      x2 = numX * invDenX
      y2 = numY * invDenY
   in AffinePoint (EC.AffinePoint x2 y2)
  where
    negJubjubD :: Natural
    negJubjubD = value @Jubjub_Base -! jubjubD

instance
  ( Symbolic ctx
  , KnownFFA Jubjub_Base 'Auto ctx
  , KnownFFA Jubjub_Scalar 'Auto ctx
  )
  => CyclicGroup (Jubjub_Point ctx)
  where
  type ScalarFieldOf (Jubjub_Point ctx) = FFA Jubjub_Scalar 'Auto ctx
  pointGen =
    pointXY
      (fromConstant (8076246640662884909881801758704306714034609987455869804520522091855516602923 :: Natural))
      (fromConstant (13262374693698910701929044844600465831413122818447359594527400194675274060458 :: Natural))

instance
  ( Symbolic ctx
  , KnownFFA Jubjub_Base 'Auto ctx
  , KnownFFA Jubjub_Scalar 'Auto ctx
  )
  => Scale (FFA Jubjub_Scalar 'Auto ctx) (Jubjub_Point ctx)
  where
  -- | Scalar multiplication using the power-of-2 method.
  -- For each bit of the scalar, selects zero or the corresponding power-of-2 multiple,
  -- then sums all selected points.
  scale ffa x =
    jubjubSum $
      Prelude.zipWith
        (jubjubSelectPoint)
        [upper, upper -! 1 .. 0]
        (Prelude.iterate jubjubDouble x)
   where
    bits :: ByteString (NumberOfBits (Zp Jubjub_Scalar)) ctx
    bits = binaryExpansion ffa

    upper :: Natural
    upper = value @(NumberOfBits (Zp Jubjub_Scalar)) -! 1
    
    -- Select zero or point based on bit value
    jubjubSelectPoint :: Natural -> Jubjub_Point ctx -> Jubjub_Point ctx
    jubjubSelectPoint bitIdx (AffinePoint (EC.AffinePoint px py)) =
      let Bool bitCircuit = isSet bits bitIdx
          bitFE = FieldElement bitCircuit
          zeroX = fromConstant (0 :: Natural)
          zeroY = fromConstant (1 :: Natural)
          resX = ffaConditionalSelect bitFE zeroX px
          resY = ffaConditionalSelect bitFE zeroY py
       in AffinePoint (EC.AffinePoint resX resY)
    
    jubjubDouble :: Jubjub_Point ctx -> Jubjub_Point ctx
    jubjubDouble p = jubjubAdd p p
    
    jubjubSum :: [Jubjub_Point ctx] -> Jubjub_Point ctx
    jubjubSum [] = zero
    jubjubSum (p:ps) = Prelude.foldl jubjubAdd p ps
