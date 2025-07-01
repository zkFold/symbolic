{-# LANGUAGE TypeOperators #-}

module Main where

import Control.DeepSeq (force)
import Control.Monad (return)
import Data.Binary (Binary)
import Data.ByteString (foldr)
import Data.Function (($), (.))
import Data.Functor.Rep (Representable (..))
import Data.String (String)
import Data.Type.Equality (type (~))
import System.IO (IO)
import Test.Tasty.Bench
import ZkFold.Algebra.Class (Ring, fromConstant, zero, (+))
import ZkFold.ArithmeticCircuit (ArithmeticCircuit, eval)
import ZkFold.ArithmeticCircuit.Context (CircuitContext)
import ZkFold.Data.ByteString (toByteString)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Compiler (compile)
import ZkFold.Symbolic.Data.Class (Context, SymbolicData)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import Prelude (toInteger)

import ZkFold.Symbolic.Examples (ExampleOutput (..), examples)

fromBinary :: (Binary a, Ring b) => a -> b
fromBinary = foldr ((+) . fromConstant . toInteger) zero . toByteString

benchmark
  :: forall a i o
   . ( Arithmetic a
     , Binary a
     , SymbolicInput i
     , Context i ~ CircuitContext a
     , SymbolicData o
     , Context o ~ CircuitContext a
     )
  => String -> (i -> o) -> Benchmark
benchmark name fun =
  bgroup
    name
    [ bench "compilation" $ nf (compile @_ @(ArithmeticCircuit a _ _)) fun
    , env (return $ force $ compile fun) $ \c ->
        bench "evaluation" $ nf (`eval` tabulate fromBinary) c
    ]

main :: IO ()
main = defaultMain [benchmark n c | (n, ExampleOutput c) <- examples]
