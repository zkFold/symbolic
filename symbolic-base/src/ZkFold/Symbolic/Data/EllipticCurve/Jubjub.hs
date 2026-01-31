{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point, jubjubAdd, jubjubConditionalAdd) where

import Data.Function (($))
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class hiding (AffinePoint)
import qualified ZkFold.Algebra.EllipticCurve.Class as EC
import ZkFold.Algebra.EllipticCurve.Jubjub (Jubjub_Base, Jubjub_Scalar)
import ZkFold.Algebra.Number
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Bool (Bool (..))
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto), from)
import ZkFold.Symbolic.Data.EllipticCurve.Point.Affine (AffinePoint (..))
import ZkFold.Symbolic.Data.FFA
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))

type Jubjub_Point = AffinePoint (TwistedEdwards "jubjub") (FFA Jubjub_Base 'Auto)

-- | Jubjub curve parameter d = -10240/10241 (mod BLS12_381_Scalar)
-- Precomputed value for efficiency
jubjubD :: Natural
jubjubD = 19257038036680949359750312669786877991949435402254120286184196891950884077233

-- | Optimized point addition on the Jubjub curve.
-- Uses common subexpression elimination to minimize the total number of constraints.
--
-- The TwistedEdwards addition formula is:
--   x3 = (x0*y1 + y0*x1) / (1 + d*x0*x1*y0*y1)
--   y3 = (y0*y1 + x0*x1) / (1 - d*x0*x1*y0*y1)  [with a = -1]
--
-- Optimized with:
--   - Common subexpression reuse: t1=x0*x1, t2=y0*y1, t3=x0*y1, t4=y0*x1, t5=t1*t2
--   - 1-constraint inversion with inlined affine denominator (no denX/denY variables)
--     Using: s*t5*inv + c*inv - 1 = 0 instead of: denX = 1+d*t5; inv*denX = 1
--
-- SAFETY: Denominators are non-zero for valid twisted Edwards curve points
-- because |d*x0*x1*y0*y1| < 1 (curve property). Use only with valid points.
jubjubAdd
  :: forall ctx
   . ( Symbolic ctx
     , KnownFFA Jubjub_Base 'Auto ctx
     )
  => Jubjub_Point ctx
  -> Jubjub_Point ctx
  -> Jubjub_Point ctx
jubjubAdd (AffinePoint (EC.AffinePoint x0 y0)) (AffinePoint (EC.AffinePoint x1 y1)) =
  let -- Common subexpressions (each costs 1 multiplication constraint)
      t1 = x0 * x1       -- x0*x1 (1 constraint)
      t2 = y0 * y1       -- y0*y1 (1 constraint)
      t3 = x0 * y1       -- x0*y1 (1 constraint)
      t4 = y0 * x1       -- y0*x1 (1 constraint)
      t5 = t1 * t2       -- x0*x1*y0*y1 (1 constraint)
      
      -- Numerators (1 constraint each)
      numX = t3 + t4     -- x0*y1 + y0*x1 (1 constraint)
      -- For a = -1: y0*y1 - a*x0*x1 = y0*y1 + x0*x1
      numY = t2 + t1     -- (1 constraint)
      
      -- Inlined inverse of affine denominator (1 constraint each)
      -- Instead of: denX = 1 + d*t5 (1 constraint), invX = 1/denX (1 constraint)
      -- We use: invX = 1/(1 + d*t5) with inlined affine function (1 constraint)
      -- Constraint: d*t5*invX + 1*invX - 1 = 0
      invDenX = ffaInvAffineOrFail (1 :: Natural) jubjubD t5     -- 1/(1 + d*t5) (1 constraint)
      invDenY = ffaInvAffineOrFail (1 :: Natural) negJubjubD t5  -- 1/(1 - d*t5) (1 constraint)
      
      -- Final multiplications (1 constraint each)
      x2 = numX * invDenX  -- (1 constraint)
      y2 = numY * invDenY  -- (1 constraint)
   in AffinePoint (EC.AffinePoint x2 y2)
  where
    -- -d (mod p) = p - d
    negJubjubD :: Natural
    negJubjubD = value @Jubjub_Base -! jubjubD

-- | Conditional point addition: if bit=1, return acc + p; if bit=0, return acc.
-- Uses efficient 1-constraint conditional selection per coordinate instead of
-- expensive ~10+ constraint interpolation.
--
-- Total cost: 11 constraints for addition + 2 constraints for selection = 13 constraints
-- Compare to: 11 + ~20 interpolation = ~31 constraints with generic `bool`
--
-- SAFETY: The bit parameter must be 0 or 1 for correct results.
jubjubConditionalAdd
  :: forall ctx
   . ( Symbolic ctx
     , KnownFFA Jubjub_Base 'Auto ctx
     )
  => FieldElement ctx         -- ^ Selector bit (0 or 1)
  -> Jubjub_Point ctx         -- ^ Accumulator point
  -> Jubjub_Point ctx         -- ^ Point to conditionally add
  -> Jubjub_Point ctx         -- ^ Result: if bit=1 then acc+p else acc
jubjubConditionalAdd bit acc@(AffinePoint (EC.AffinePoint accX accY)) p =
  let -- Compute the sum (always, 11 constraints)
      sumPoint = jubjubAdd acc p
      AffinePoint (EC.AffinePoint sumX sumY) = sumPoint
      
      -- Select based on bit (1 constraint each, total 2 constraints)
      resultX = ffaConditionalSelect bit accX sumX
      resultY = ffaConditionalSelect bit accY sumY
  in AffinePoint (EC.AffinePoint resultX resultY)

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
  -- | Optimized scalar multiplication using the power-of-2 method with efficient conditional selection.
  -- Instead of expensive interpolation-based `bool`, uses 1-constraint selection per coordinate.
  --
  -- Algorithm:
  --   1. Precompute powers: [x, 2x, 4x, 8x, ...]
  --   2. For each bit, select zero or the corresponding power
  --   3. Sum all selected points
  scale ffa x =
    jubjubSum $
      Prelude.zipWith
        (jubjubSelectPoint)
        [upper, upper -! 1 .. 0]
        (Prelude.iterate jubjubDouble x)
   where
    bits :: ByteString (FFAMaxBits Jubjub_Scalar ctx) ctx
    bits = from (toUInt @(FFAMaxBits Jubjub_Scalar ctx) ffa)

    upper :: Natural
    upper = value @(FFAMaxBits Jubjub_Scalar ctx) -! 1
    
    -- Efficient point selection: if bit=1 return p, else return zero
    -- Uses 2 constraints (1 per coordinate) instead of ~20 interpolation constraints
    jubjubSelectPoint :: Natural -> Jubjub_Point ctx -> Jubjub_Point ctx
    jubjubSelectPoint bitIdx p@(AffinePoint (EC.AffinePoint px py)) =
      let Bool bitCircuit = isSet bits bitIdx
          bitFE = FieldElement bitCircuit
          -- zero = (0, 1) on twisted Edwards
          zeroX = fromConstant (0 :: Natural)
          zeroY = fromConstant (1 :: Natural)
          -- Select: bit=0 -> zero, bit=1 -> p
          resX = ffaConditionalSelect bitFE zeroX px
          resY = ffaConditionalSelect bitFE zeroY py
       in AffinePoint (EC.AffinePoint resX resY)
    
    -- Optimized point doubling using jubjubAdd
    jubjubDouble :: Jubjub_Point ctx -> Jubjub_Point ctx
    jubjubDouble p = jubjubAdd p p
    
    -- Optimized sum using jubjubAdd
    jubjubSum :: [Jubjub_Point ctx] -> Jubjub_Point ctx
    jubjubSum [] = zero
    jubjubSum (p:ps) = Prelude.foldl jubjubAdd p ps
