{-# LANGUAGE TypeApplications #-}

module Examples.Conditional (exampleConditional) where

import ZkFold.Base.Algebra.Basic.Field (Zp)
import ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Symbolic.Compiler
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.Conditional (Conditional (..))
import Prelude (IO, putStrLn)

type F = Zp BLS12_381_Scalar

type A = ArithmeticCircuit F

type B = Bool A

exampleConditional :: IO ()
exampleConditional = do
  let file = "compiled_scripts/conditional.json"

  putStrLn "\nExample: conditional\n"

  compileIO @F file (bool @B @A)
