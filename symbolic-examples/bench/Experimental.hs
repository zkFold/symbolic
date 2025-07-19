{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.DeepSeq (force)
import Control.Monad (return)
import Data.ByteString.Lazy (ByteString)
import Data.Function (($), (.))
import Data.Functor.Rep (tabulate)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.String qualified as String
import System.IO (IO)
import Test.Tasty.Bench
import Test.Tasty.Golden (goldenVsString)
import Text.Show (show)
import ZkFold.Algebra.Class (zero)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit, eval)
import ZkFold.ArithmeticCircuit qualified as Circuit
import ZkFold.ArithmeticCircuit.Experimental (compile)

import ZkFold.Symbolic.Examples.MiMCHash (exampleMiMC)

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

type A = Zp BLS12_381_Scalar

main :: IO ()
main =
  defaultMain
    [ bench "MiMC compilation" $ nf (compile @A) exampleMiMC
    , env (return $ force $ compile @A exampleMiMC) $
        bench "MiMC evaluation" . nf (`eval` tabulate zero)
    , goldenVsString "MiMC golden stats" "stats/Experimental.MiMC" do
        return $ metrics "Experimental.MiMC" (compile @A exampleMiMC)
    ]
