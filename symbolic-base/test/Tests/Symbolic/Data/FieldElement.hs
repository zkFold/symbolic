{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module Tests.Symbolic.Data.FieldElement (specFieldElement) where

import Data.Function (flip, ($))
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Instances ()
import Prelude (Integer)

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Testing

specFieldElement' :: forall p. Arithmetic (Zp p) => Spec
specFieldElement' = describe "FieldElement specification" do
  ringModelAxioms @FieldElement @(Zp p) arbitrary arbitrary
  it "models inversion" $ commutative1 @(FieldElement (Zp p)) finv finv arbitrary
  it "models division" $ commutative2 @(FieldElement (Zp p)) (//) (//) arbitrary
  it "models exponentiation" $
    commutativePar @Integer @(FieldElement (Zp p))
      (flip (^))
      (flip (^))
      arbitrary

-- Type-specific tests go here

specFieldElement :: Spec
specFieldElement = do
  specFieldElement' @BLS12_381_Scalar
