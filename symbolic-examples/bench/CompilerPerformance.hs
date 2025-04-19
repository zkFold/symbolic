module Main where

import           Control.DeepSeq          (NFData, NFData1, force)
import           Control.Monad            (return)
import           Data.Binary              (Binary)
import           Data.Function            (const, ($))
import           Data.Functor.Rep         (Representable (..))
import           Data.String              (String)
import           System.IO                (IO)
import           Test.Tasty.Bench

import           ZkFold.Algebra.Class     (zero)
import           ZkFold.Symbolic.Class    (Arithmetic)
import           ZkFold.Symbolic.Compiler
import           ZkFold.Symbolic.Examples (ExampleOutput (..), examples)

benchmark ::
  (Arithmetic a, Binary a, NFData (Rep i), Representable i, NFData1 o) =>
  String -> (() -> ArithmeticCircuit a i o) -> Benchmark
benchmark name circuit = bgroup name
  [ bench "compilation" $ nf circuit ()
  , env (return $ force $ circuit ()) $ \c ->
    let input = tabulate (const zero)
     in bench "evaluation" $ nf (witnessGenerator c) input
  ]

main :: IO ()
main = defaultMain [ benchmark n c | (n, ExampleOutput c) <- examples ]
