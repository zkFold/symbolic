{-# LANGUAGE TypeOperators #-}

module Tests.Symbolic.Compiler (specCompiler) where

import Data.Function (id, ($))
import GHC.Generics (U1, (:*:))
import Test.Hspec (Spec, describe)

import Tests.Symbolic.Compiler.Optimization (specOptimization)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.ArithmeticCircuit.Elem (Elem, compile)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.UInt

type A = Zp BLS12_381_Scalar

type C = Elem A

specCompiler :: Spec
specCompiler = do
  describe "Compiler specification" $ do
    let _ =
          -- compile-time test
          compile @A id $ id @((FieldElement :*: U1 :*: UInt 32) C)
    specOptimization @A
