module Tests.Symbolic.Compiler (specCompiler) where

import           Data.Function                          (($))
import           Test.Hspec                             (Spec, describe)
import           Tests.Symbolic.Compiler.CompileWith    (specCompileWith)
import           Tests.Symbolic.Compiler.Optimization   (specOptimization)

import           ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import           ZkFold.Algebra.Field                   (Zp)

specCompiler :: Spec
specCompiler = do
  describe "Compiler specification" $ do
    specCompileWith @(Zp BLS12_381_Scalar)
    specOptimization @(Zp BLS12_381_Scalar)
