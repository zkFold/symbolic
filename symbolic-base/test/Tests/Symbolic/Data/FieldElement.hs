{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Symbolic.Data.FieldElement (specFieldElement) where

import           Data.Function                               (($))
import           Data.List                                   ((++))
import           GHC.Generics                                (U1)
import           Prelude                                     (Integer)
import           Test.Hspec                                  (Spec, describe)
import           Test.QuickCheck                             (Property, (===))
import           Tests.Symbolic.ArithmeticCircuit            (it)
import           Tests.Symbolic.Data.Common                  (specSymbolicData)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit, exec)
import           ZkFold.Symbolic.Data.FieldElement           (FieldElement (FieldElement))
import           ZkFold.Symbolic.Interpreter                 (Interpreter (Interpreter))

type Binary a = a -> a -> a
type Predicate a = a -> a -> Property

isHom ::
  (PrimeField (Zp p)) =>
  Binary (FieldElement (Interpreter (Zp p))) ->
  Binary (FieldElement (ArithmeticCircuit (Zp p) U1)) -> Predicate (Zp p)
isHom f g x y = execAcFieldElement (fromConstant x `g` fromConstant y) === execInterpreterFieldElement (fromConstant x `f` fromConstant y)

execAcFieldElement ::
  forall p .
  (PrimeField (Zp p)) =>
  FieldElement (ArithmeticCircuit (Zp p) U1) -> Zp p
execAcFieldElement (FieldElement x) =
  toConstant $ (FieldElement $ Interpreter $ exec x)

execInterpreterFieldElement ::
  forall p . FieldElement (Interpreter (Zp p)) -> Zp p
execInterpreterFieldElement = toConstant

specFieldElement' :: forall p . (PrimeField (Zp p)) => Spec
specFieldElement' = do
  describe ("FieldElement " ++ " specification") $ do
    it "FieldElement embeds Zp" $ \(x :: Zp p) ->
      toConstant (fromConstant x :: FieldElement (Interpreter (Zp p))) === x
    it "Zp embeds FieldElement" $ \(x :: FieldElement (Interpreter (Zp p))) ->
      fromConstant (toConstant x :: Zp p) === x
    it "has zero" $ execAcFieldElement @p zero === execInterpreterFieldElement zero
    it "has one" $ execAcFieldElement @p one === execInterpreterFieldElement one
    it "adds correctly" $ isHom @p (+) (+)
    specSymbolicData @(Zp p) @(FieldElement) "negates correctly" negate
    it "multiplies correctly" $ isHom @p (*) (*)
    specSymbolicData @(Zp p) @(FieldElement) "inverts correctly" finv
    it "divides correctly" $ isHom @p (//) (//)
    it "powers correctly" $ \(x :: Zp p) (e :: Integer) ->
      execAcFieldElement @p (fromConstant x ^ e) === x ^ e

specFieldElement :: Spec
specFieldElement = do
  specFieldElement' @BLS12_381_Scalar
