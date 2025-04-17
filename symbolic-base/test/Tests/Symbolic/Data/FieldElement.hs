{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Symbolic.Data.FieldElement (specFieldElement) where

import           Data.Function                               (($), id)
import           Data.List                                   ((++))
import           Prelude                                     (Integer)
import           Test.Hspec                                  (Spec, describe)
import           Tests.Symbolic.Data.Common                  (specSymbolicFunction1, specConstantRoundtrip, specSymbolicFunction2, specSymbolicFunction1WithPar)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import           ZkFold.Symbolic.Data.FieldElement           (FieldElement)

specFieldElement' :: forall p . (PrimeField (Zp p)) => Spec
specFieldElement' = do
  describe ("FieldElement" ++ " specification") $ do
    specConstantRoundtrip @(Zp p) @FieldElement "FieldElement" "Zp"
    specSymbolicFunction1 @(Zp p) @FieldElement "identity" id
    specSymbolicFunction2 @(Zp p) @FieldElement "addition" (+)
    specSymbolicFunction1 @(Zp p) @FieldElement "negation" negate
    specSymbolicFunction2 @(Zp p) @FieldElement "subtraction" (-)
    specSymbolicFunction2 @(Zp p) @FieldElement "multiplication" (*)
    specSymbolicFunction1 @(Zp p) @FieldElement "inversion" finv
    specSymbolicFunction2 @(Zp p) @FieldElement "division" (//)
    specSymbolicFunction1WithPar @Integer @(Zp p) @FieldElement "exponentiation" (\e x -> x ^ e)
    -- Type-specific tests go here

specFieldElement :: Spec
specFieldElement = do
  specFieldElement' @BLS12_381_Scalar
