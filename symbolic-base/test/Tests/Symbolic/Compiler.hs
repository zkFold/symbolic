{-# LANGUAGE TypeOperators #-}

module Tests.Symbolic.Compiler (specCompiler) where

import Data.Function (id, ($))
import Data.Proxy (Proxy)
import GHC.Generics ((:*:))
import Test.Hspec (Spec, describe)

import Tests.Symbolic.Compiler.CompileWith (specCompileWith)
import Tests.Symbolic.Compiler.Optimization (specOptimization)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.ArithmeticCircuit.Context (CircuitContext)
import ZkFold.Symbolic.Compiler (compileIO)
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.UInt (UInt)

type A = Zp BLS12_381_Scalar

type C = CircuitContext A

specCompiler :: Spec
specCompiler = do
  describe "Compiler specification" $ do
    let _ =
          -- compile-time test
          compileIO @A "ex" $ id @((FieldElement :*: Proxy :*: UInt 32 Auto) C)
    specCompileWith @A
    specOptimization @A
