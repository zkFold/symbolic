{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Symbolic.Compiler (specCompiler) where

import           Data.Function                               (($))
import           Test.Hspec                                  (Spec, describe)
import           Tests.Symbolic.Compiler.CompileWith         (specCompileWith)
import           Tests.Symbolic.Compiler.Optimization        (specOptimization)

import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)

specCompiler :: Spec
specCompiler = do
  describe "Compiler specification" $ do
    specCompileWith @(Zp BLS12_381_Scalar)
    specOptimization @(Zp BLS12_381_Scalar)
