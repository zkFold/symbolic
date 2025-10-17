{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}

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
import Data.Type.Equality (type (~))
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
import ZkFold.ArithmeticCircuit.Elem (compile)
import ZkFold.ArithmeticCircuit.Node (compileV1)
import ZkFold.Data.Binary (Binary, toByteString)
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.UInt (UInt)
import Prelude (toInteger)

import ZkFold.Symbolic.Examples.Fibonacci (exampleFibonacciMod)
import ZkFold.Symbolic.Examples.MiMCHash (exampleMiMC)
import ZkFold.Symbolic.Examples.UInt (exampleUIntExpMod)
import ZkFold.Symbolic.Class (Symbolic, BaseField)

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

fib100 :: Symbolic c => FieldElement c -> FieldElement c
fib100 = exampleFibonacciMod 100

expMod ::
  (Symbolic c, BaseField c ~ A) =>
  UInt 32 Auto c -> UInt 16 Auto c -> UInt 64 Auto c -> UInt 64 Auto c
expMod = exampleUIntExpMod

main :: IO ()
main =
  defaultMain
    [ testGroup
        "MiMCHash"
        [ bench "compilation (Node)" $ nf (compileV1 @A) exampleMiMC
        , bench "compilation (Elem)" $ nf (compile @A) exampleMiMC
        , env (return $ force $ compileV1 @A exampleMiMC) $
            bench "evaluation (Node)" . nf (`eval` tabulate zero)
        , env (return $ force $ compile @A exampleMiMC) $
            bench "evaluation (Elem)" . nf (`eval` tabulate zero)
        , goldenVsString "golden stats (Node)" "stats/Experimental.MiMC.Node" do
            return $ metrics "Experimental.MiMC.Node" (compileV1 @A exampleMiMC)
        , goldenVsString "golden stats (Elem)" "stats/Experimental.MiMC.Elem" do
            return $ metrics "Experimental.MiMC.Elem" (compile @A exampleMiMC)
        ]
    , testGroup
        "Fib100"
        [ bench "compilation (Node)" $ nf (compileV1 @A) fib100
        , bench "compilation (Elem)" $ nf (compile @A) fib100
        , env (return $ force $ compileV1 @A fib100) $
            bench "evaluation (Node)" . nf (`eval` tabulate fromBinary)
        , env (return $ force $ compile @A fib100) $
            bench "evaluation (Elem)" . nf (`eval` tabulate fromBinary)
        , goldenVsString "golden stats (Node)" "stats/Experimental.Fib100.Node" do
            return $ metrics "Experimental.Fib100.Node" (compileV1 @A fib100)
        , goldenVsString "golden stats (Elem)" "stats/Experimental.Fib100.Elem" do
            return $ metrics "Experimental.Fib100.Elem" (compile @A fib100)
        ]
    , testGroup
        "ExpMod"
        [ bench "compilation (Node)" $ nf (compileV1 @A) expMod
        , bench "compilation (Elem)" $ nf (compile @A) expMod
        , env (return $ force $ compileV1 @A expMod) $
            bench "evaluation (Node)" . nf (`eval` tabulate fromBinary)
        , env (return $ force $ compile @A expMod) $
            bench "evaluation (Elem)" . nf (`eval` tabulate fromBinary)
        , goldenVsString "golden stats (Node)" "stats/Experimental.ExpMod.Node" do
            return $ metrics "Experimental.ExpMod.Node" (compileV1 @A expMod)
        , goldenVsString "golden stats (Elem)" "stats/Experimental.ExpMod.Elem" do
            return $ metrics "Experimental.ExpMod.Elem" (compile @A expMod)
        ]
    ]
