{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.DeepSeq (NFData)
import GHC.Generics (U1)
import System.Random (randomRIO)
import Test.Tasty.Bench
import Prelude hiding (sum, (*), (+), (-), (/), (^))

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.EllipticCurve.Ed25519 hiding (Ed25519_Point)
import ZkFold.Algebra.Field
import ZkFold.Algebra.Number
import ZkFold.ArithmeticCircuit
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto))
import ZkFold.Symbolic.Data.EllipticCurve.Ed25519 (Ed25519_Point)
import ZkFold.Symbolic.Data.FFA
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.UInt (UInt (..))
import ZkFold.Symbolic.Interpreter

type I = Interpreter (Zp BLS12_381_Scalar)

type A = ArithmeticCircuit (Zp BLS12_381_Scalar) U1

type PtFFA c = Ed25519_Point c

benchOps :: NFData a => String -> a -> (Natural -> a -> a) -> Benchmark
benchOps desc p0 op = env (fromIntegral <$> randomRIO (1 :: Integer, 3)) $ \n ->
  bgroup "Point scaling" [bench desc $ nf (`op` p0) n]

main :: IO ()
main = do
  let a = fromConstant @Natural 0 :: FFA Ed25519_Scalar Auto A
  --  let b = fromConstant @Natural 1 :: FFA Ed25519_Scalar A
  print a
  let (FFA (FieldElement nx) (UInt ux)) = a ^ (100000 :: Natural)
  --  let (FFA ap) = (scale (100000 :: Natural) a)
  print $ exec nx
  print $ exec ux
  print a

--  print $ a // a
--  print $ b // a
--  defaultMain
--      [ bgroup "EC operations"
--        [ benchOps "FFA Interpreter" (gen :: PtFFA I) scale
--        , benchOps "FFA Circuit" (gen :: PtFFA A) scale
--        ]
--      ]
