{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Symbolic.Data.FieldElement (specFieldElement) where

import           Data.Function                               (($))
import           Data.List                                   ((++))
import           Prelude                                     (Integer)
import           Test.Hspec                                  (Spec, describe)
import           Tests.Symbolic.Data.Common                  (specSymbolicFunction1, specSymbolicFunction2, specSymbolicFunction1WithPar, specSymbolicData, specConstantRoundtrip)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import           ZkFold.Symbolic.Data.FieldElement           (FieldElement)

specFieldElement' :: forall p . (PrimeField (Zp p)) => Spec
specFieldElement' = do
  describe ("FieldElement " ++ " specification") $ do
    specConstantRoundtrip @(Zp p) @FieldElement "FieldElement" "Zp"
    specSymbolicData @(Zp p) @FieldElement "compiles correctly"
    specSymbolicFunction2 @(Zp p) @FieldElement "adds correctly" (+)
    specSymbolicFunction1 @(Zp p) @FieldElement "negates correctly" negate
    specSymbolicFunction2 @(Zp p) @FieldElement "subtracts correctly" (-)
    specSymbolicFunction2 @(Zp p) @FieldElement "multiplies correctly" (*)
    specSymbolicFunction1 @(Zp p) @FieldElement "inverts correctly" finv
    specSymbolicFunction2 @(Zp p) @FieldElement "divides correctly" (//)
    specSymbolicFunction1WithPar @Integer @(Zp p) @FieldElement "powers correctly" (\e x -> x ^ e)
    -- Type-specific tests go here

specFieldElement :: Spec
specFieldElement = do
  specFieldElement' @BLS12_381_Scalar
