{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Symbolic.Data.FieldElement (specFieldElement) where

import Data.Function (id, ($))
import Data.List ((++))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (arbitrary)
import Prelude (Integer)

import Tests.Symbolic.Data.Common (
  specConstantRoundtrip,
  specSymbolicFunction0,
  specSymbolicFunction1,
  specSymbolicFunction1WithPar,
  specSymbolicFunction2,
 )
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)

specFieldElement' :: forall p. PrimeField (Zp p) => Spec
specFieldElement' = do
  describe ("FieldElement" ++ " specification") $ do
    specConstantRoundtrip @(Zp p) @FieldElement "FieldElement" "Zp" arbitrary
    specSymbolicFunction1 @(Zp p) @FieldElement "identity" id
    specSymbolicFunction0 @(Zp p) @FieldElement "zero" zero
    specSymbolicFunction2 @(Zp p) @FieldElement "addition" (+)
    specSymbolicFunction1 @(Zp p) @FieldElement "negation" negate
    specSymbolicFunction2 @(Zp p) @FieldElement "subtraction" (-)
    specSymbolicFunction0 @(Zp p) @FieldElement "one" one
    specSymbolicFunction2 @(Zp p) @FieldElement "multiplication" (*)
    specSymbolicFunction1 @(Zp p) @FieldElement "inversion" finv
    specSymbolicFunction2 @(Zp p) @FieldElement "division" (//)
    specSymbolicFunction1WithPar @Integer @(Zp p) @FieldElement "exponentiation" (\e x -> x ^ e)

-- Type-specific tests go here

specFieldElement :: Spec
specFieldElement = do
  specFieldElement' @BLS12_381_Scalar
