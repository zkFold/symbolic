{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples.Jubjub (
  exampleJubjubAdd,
  exampleJubjubScale,
  exampleJubjubEdDSA,
) where

import GHC.Generics ((:*:))
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..), TwistedEdwards)
import ZkFold.Algebra.EllipticCurve.Jubjub (Jubjub_Base, Jubjub_Scalar)
import ZkFold.Symbolic.Algorithm.EdDSA (eddsaVerify)
import ZkFold.Symbolic.Algorithm.Hash.MiMC (hash)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto))
import ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point, jubjubAdd)
import qualified ZkFold.Symbolic.Data.EllipticCurve.Point.Affine as SymAffine
import ZkFold.Symbolic.Data.FFA
import ZkFold.Symbolic.Data.FieldElement (FieldElement)

-- | Point addition on the Jubjub curve.
-- Uses the TwistedEdwards addition formula:
--   x3 = (x1*y2 + y1*x2) / (1 + d*x1*x2*y1*y2)
--   y3 = (y1*y2 - a*x1*x2) / (1 - d*x1*x2*y1*y2)
exampleJubjubAdd
  :: ( Symbolic ctx
     , KnownFFA Jubjub_Base 'Auto ctx
     )
  => Jubjub_Point ctx
  -> Jubjub_Point ctx
  -> Jubjub_Point ctx
exampleJubjubAdd = jubjubAdd

exampleJubjubScale
  :: ( Symbolic ctx
     , KnownFFA Jubjub_Base 'Auto ctx
     , KnownFFA Jubjub_Scalar 'Auto ctx
     )
  => ScalarFieldOf (Jubjub_Point ctx)
  -> Jubjub_Point ctx
  -> Jubjub_Point ctx
exampleJubjubScale = scale

exampleJubjubEdDSA
  :: ( Symbolic ctx
     , KnownFFA Jubjub_Base 'Auto ctx
     , KnownFFA Jubjub_Scalar 'Auto ctx
     )
  => Jubjub_Point ctx
  -> FieldElement ctx
  -> (SymAffine.AffinePoint (TwistedEdwards "jubjub") (FFA Jubjub_Base 'Auto) :*: FFA Jubjub_Scalar 'Auto) ctx
  -> Bool ctx
exampleJubjubEdDSA = eddsaVerify @_ @"jubjub" hash
