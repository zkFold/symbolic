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
import ZkFold.Algebra.Number
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto), from)
import ZkFold.Symbolic.Data.EllipticCurve.Point.Affine (AffinePoint (..))
import ZkFold.Symbolic.Data.FFA

type Jubjub_Point = AffinePoint (TwistedEdwards "jubjub") (FFA Jubjub_Base 'Auto)

-- | Jubjub curve parameter d = -10240/10241 (mod BLS12_381_Scalar)
-- Precomputed value for efficiency
jubjubD :: Natural
jubjubD = 19257038036680949359750312669786877991949435402254120286184196891950884077233

-- | Optimized point addition on the Jubjub curve.
-- Uses common subexpression elimination to minimize constraints.
--
-- The TwistedEdwards addition formula is:
--   x3 = (x1*y2 + y1*x2) / (1 + d*x1*x2*y1*y2)
--   y3 = (y1*y2 - a*x1*x2) / (1 - d*x1*x2*y1*y2)
--
-- For Jubjub: a = -1, d = -10240/10241
--
-- Optimized to reuse:
--   t1 = x1*x2, t2 = y1*y2, t3 = x1*y2, t4 = y1*x2
--   t5 = t1*t2 (= x1*x2*y1*y2)
--   dt5 = d*t5
--
-- Also uses ffaDivOrFail for 1-constraint inversion (vs 2 for regular finv).
-- The denominators are guaranteed non-zero for valid curve points on twisted
-- Edwards curves because |d*x1*x2*y1*y2| < 1 for any valid points (due to the
-- curve equation constraints). This function should only be used with valid
-- curve points, not with arbitrary field elements or the identity point.
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
      t1 = x0 * x1       -- x0*x1
      t2 = y0 * y1       -- y0*y1
      t3 = x0 * y1       -- x0*y1
      t4 = y0 * x1       -- y0*x1
      t5 = t1 * t2       -- x0*x1*y0*y1
      
      -- d*t5 using scale (still costs 1 constraint in current framework)
      -- d = -10240/10241 (mod BLS12_381_Scalar) = jubjubD
      dt5 = scale jubjubD t5  -- d*x0*x1*y0*y1
      
      -- Numerators (cost 1 constraint each for variable assignment)
      numX = t3 + t4     -- x0*y1 + y0*x1
      -- For a = -1: y0*y1 - a*x0*x1 = y0*y1 - (-1)*x0*x1 = y0*y1 + x0*x1
      numY = t2 + t1
      
      -- Denominators (cost 1 constraint each for variable assignment)
      denX = one + dt5   -- 1 + d*x0*x1*y0*y1
      denY = one - dt5   -- 1 - d*x0*x1*y0*y1
      
      -- Final divisions using optimized 1-constraint inversion
      -- (each costs 2 constraints: 1 for inversion + 1 for multiplication)
      x2 = ffaDivOrFail numX denX
      y2 = ffaDivOrFail numY denY
   in AffinePoint (EC.AffinePoint x2 y2)

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
  -- | Scalar multiplication using double-and-add with optimized point addition.
  -- Uses the optimized jubjubAdd which reduces constraints per addition.
  scale ffa x =
    jubjubSum $
      Prelude.zipWith
        (\b p -> bool @(Bool ctx) zero p (isSet bits b))
        [upper, upper -! 1 .. 0]
        (Prelude.iterate jubjubDouble x)
   where
    bits :: ByteString (FFAMaxBits Jubjub_Scalar ctx) ctx
    bits = from (toUInt @(FFAMaxBits Jubjub_Scalar ctx) ffa)

    upper :: Natural
    upper = value @(FFAMaxBits Jubjub_Scalar ctx) -! 1
    
    -- Optimized point doubling using jubjubAdd
    jubjubDouble :: Jubjub_Point ctx -> Jubjub_Point ctx
    jubjubDouble p = jubjubAdd p p
    
    -- Optimized sum using jubjubAdd
    jubjubSum :: [Jubjub_Point ctx] -> Jubjub_Point ctx
    jubjubSum [] = zero
    jubjubSum (p:ps) = Prelude.foldl jubjubAdd p ps
