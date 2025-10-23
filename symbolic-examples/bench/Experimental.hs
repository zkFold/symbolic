{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.DeepSeq (force)
import Control.Monad (return)
import Data.ByteString (foldr)
import Data.ByteString.Lazy (ByteString)
import Data.Function (id, ($), (.))
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
import ZkFold.ArithmeticCircuit.Elem (Elem)
import ZkFold.ArithmeticCircuit.Elem qualified as Elem
import ZkFold.ArithmeticCircuit.Node (Node)
import ZkFold.ArithmeticCircuit.Node qualified as Node
import ZkFold.ArithmeticCircuit.Op (Sort (ZZp))
import ZkFold.Data.Binary (Binary, toByteString)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.UInt (UInt)
import Prelude (toInteger)

import ZkFold.Symbolic.Examples.Fibonacci (exampleFibonacciMod)
import ZkFold.Symbolic.Examples.MiMCHash (exampleMiMC)
import ZkFold.Symbolic.Examples.UInt (exampleUIntExpMod)

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

expMod
  :: (Symbolic c, Order c ~ Order A)
  => UInt 32 c -> UInt 16 c -> UInt 64 c -> UInt 64 c
expMod = exampleUIntExpMod

type N = Node (Order A) ZZp

type E = Elem A

main :: IO ()
main =
  defaultMain
    [ testGroup
        "MiMCHash"
        [ bench "compilation (Node)" $ nf (Node.compile @A) (exampleMiMC @N)
        , bench "compilation (Elem)" $ nf (Elem.compile @A id) (exampleMiMC @E)
        , env (return $ force $ Node.compile @A $ exampleMiMC @N) $
            bench "evaluation (Node)" . nf (`eval` tabulate zero)
        , env (return $ force $ Elem.compile @A id $ exampleMiMC @E) $
            bench "evaluation (Elem)" . nf (`eval` tabulate zero)
        , goldenVsString "golden stats (Node)" "stats/Experimental.MiMC.Node" do
            return $ metrics "Experimental.MiMC.Node" (Node.compile @A $ exampleMiMC @N)
        , goldenVsString "golden stats (Elem)" "stats/Experimental.MiMC.Elem" do
            return $ metrics "Experimental.MiMC.Elem" (Elem.compile @A id $ exampleMiMC @E)
        ]
    , testGroup
        "Fib100"
        [ bench "compilation (Node)" $ nf (Node.compile @A) (fib100 @N)
        , bench "compilation (Elem)" $ nf (Elem.compile @A id) (fib100 @E)
        , env (return $ force $ Node.compile @A $ fib100 @N) $
            bench "evaluation (Node)" . nf (`eval` tabulate fromBinary)
        , env (return $ force $ Elem.compile @A id $ fib100 @E) $
            bench "evaluation (Elem)" . nf (`eval` tabulate fromBinary)
        , goldenVsString "golden stats (Node)" "stats/Experimental.Fib100.Node" do
            return $ metrics "Experimental.Fib100.Node" (Node.compile @A $ fib100 @N)
        , goldenVsString "golden stats (Elem)" "stats/Experimental.Fib100.Elem" do
            return $ metrics "Experimental.Fib100.Elem" (Elem.compile @A id $ fib100 @E)
        ]
    , testGroup
        "ExpMod"
        [ bench "compilation (Node)" $ nf (Node.compile @A) (expMod @N)
        , bench "compilation (Elem)" $ nf (Elem.compile @A id) (expMod @E)
        , env (return $ force $ Node.compile @A $ expMod @N) $
            bench "evaluation (Node)" . nf (`eval` tabulate fromBinary)
        , env (return $ force $ Elem.compile @A id $ expMod @E) $
            bench "evaluation (Elem)" . nf (`eval` tabulate fromBinary)
        , goldenVsString "golden stats (Node)" "stats/Experimental.ExpMod.Node" do
            return $ metrics "Experimental.ExpMod.Node" (Node.compile @A $ expMod @N)
        , goldenVsString "golden stats (Elem)" "stats/Experimental.ExpMod.Elem" do
            return $ metrics "Experimental.ExpMod.Elem" (Elem.compile @A id $ expMod @E)
        ]
    ]
