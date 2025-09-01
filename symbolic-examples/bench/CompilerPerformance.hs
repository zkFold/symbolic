module Main where

import Control.DeepSeq (NFData, force)
import Control.Monad (return)
import Data.Binary (Binary)
import Data.ByteString (foldr)
import Data.Function (($), (.))
import Data.Functor.Rep (Representable (..))
import Data.String (String)
import System.IO (IO)
import Test.Tasty.Bench
import ZkFold.Algebra.Class (Ring, fromConstant, zero, (+), Order)
import ZkFold.ArithmeticCircuit (eval)
import ZkFold.ArithmeticCircuit.Context (CircuitContext)
import ZkFold.Data.Binary (toByteString)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Compiler (compile)
import ZkFold.Symbolic.Data.Class (Layout, HasRep, SymbolicData, RepData)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Vec (Vec, runVec)
import Prelude (toInteger)

import ZkFold.Symbolic.Examples (ExampleOutput (..), examples)

fromBinary :: (Binary a, Ring b) => a -> b
fromBinary = foldr ((+) . fromConstant . toInteger) zero . toByteString

benchmark
  :: forall a i o
   . ( Arithmetic a
     , Binary a
     , SymbolicInput i
     , HasRep i (CircuitContext a)
     , RepData i (CircuitContext a)
     , SymbolicData o
     , NFData (Layout o (Order a) a)
     )
  => String -> (i (CircuitContext a) -> o (CircuitContext a)) -> Benchmark
benchmark name fun =
  bgroup
    name
    [ bench "compilation" $ nf (compile @_ @(Vec _)) fun
    , env (return $ force $ runVec $ compile fun) $ \c ->
        bench "evaluation" $ nf (`eval` tabulate fromBinary) c
    ]

main :: IO ()
main = defaultMain [benchmark n c | (n, ExampleOutput c) <- examples]
