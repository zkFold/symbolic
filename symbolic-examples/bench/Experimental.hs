{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.DeepSeq (force)
import Control.Monad (return)
import Data.ByteString (foldr)
import Data.ByteString.Lazy (ByteString)
import Data.Function (($), (.))
import Data.Functor.Rep (tabulate)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.String qualified as String
import System.IO (IO)
import Test.Tasty (testGroup)
import Test.Tasty.Bench
import Test.Tasty.Golden (goldenVsString)
import Text.Show (show)
import ZkFold.Algebra.Class hiding (fromBinary)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit, eval)
import ZkFold.ArithmeticCircuit qualified as Circuit
import ZkFold.Data.Binary (Binary, toByteString)
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.UInt (UInt)
import Prelude (toInteger)

import ZkFold.Symbolic.Examples.Fibonacci (exampleFibonacciMod)
import ZkFold.Symbolic.Examples.MiMCHash (exampleMiMC)
import ZkFold.Symbolic.Examples.UInt (exampleUIntExpMod)
import ZkFold.Symbolic.Compat (CompatContext)
import ZkFold.ArithmeticCircuit.Experimental (Node, compileV1)
import ZkFold.ArithmeticCircuit.Op (Sort(ZZp))

metrics :: String -> ArithmeticCircuit a i o -> ByteString
metrics name circuit =
  String.fromString name
    <> "\nNumber of polynomial constraints: "
    <> String.fromString (show $ Circuit.acSizeN circuit)
    <> "\nNumber of variables: "
    <> String.fromString (show $ Circuit.acSizeM circuit)
    <> "\nNumber of lookup constraints: "
    <> String.fromString (show $ Circuit.acSizeL circuit)
    <> "\nNumber of lookup tables: "
    <> String.fromString (show $ Circuit.acSizeT circuit)

fromBinary :: (Binary a, Ring b) => a -> b
fromBinary = foldr ((+) . fromConstant . toInteger) zero . toByteString

type A = Zp BLS12_381_Scalar

type C = CompatContext (Node BLS12_381_Scalar ZZp)

expMod :: UInt 32 Auto C -> UInt 16 Auto C -> UInt 64 Auto C -> UInt 64 Auto C
expMod = exampleUIntExpMod

fib100 :: FieldElement C -> FieldElement C
fib100 = exampleFibonacciMod 100

main :: IO ()
main =
  defaultMain
    [ testGroup
        "MiMCHash"
        [ bench "compilation" $ nf (compileV1 @A) exampleMiMC
        , env (return $ force $ compileV1 @A exampleMiMC) $
            bench "evaluation" . nf (`eval` tabulate zero)
        , goldenVsString "golden stats" "stats/Experimental.MiMC" do
            return $ metrics "Experimental.MiMC" (compileV1 @A exampleMiMC)
        ]
    , testGroup
        "Fib100"
        [ bench "compilation" $ nf (compileV1 @A) fib100
        , env (return $ force $ compileV1 @A fib100) $
            bench "evaluation" . nf (`eval` tabulate fromBinary)
        , goldenVsString "golden stats" "stats/Experimental.Fib100" do
            return $ metrics "Experimental.Fib100" (compileV1 @A fib100)
        ]
    , testGroup
        "ExpMod"
        [ bench "compilation" $ nf (compileV1 @A) expMod
        , env (return $ force $ compileV1 @A expMod) $
            bench "evaluation" . nf (`eval` tabulate fromBinary)
        , goldenVsString "golden stats" "stats/Experimental.ExpMod" do
            return $ metrics "Experimental.ExpMod" (compileV1 @A expMod)
        ]
    ]
