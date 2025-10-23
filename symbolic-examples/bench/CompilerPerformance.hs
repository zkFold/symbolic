{-# LANGUAGE TypeOperators #-}

module Main where

import Control.DeepSeq (NFData, force)
import Control.Monad (return)
import Data.Binary (Binary)
import Data.ByteString (foldr)
import Data.Function (id, ($), (.))
import Data.Functor.Rep (Representable (..))
import Data.String (String)
import Data.Type.Equality (type (~))
import System.IO (IO)
import Test.Tasty.Bench
import ZkFold.Algebra.Class (Ring, fromConstant, zero, (+))
import ZkFold.ArithmeticCircuit (eval)
import ZkFold.ArithmeticCircuit.Elem (Elem, compile)
import ZkFold.ArithmeticCircuit.Node (Output, SymbolicFunction)
import ZkFold.ArithmeticCircuit.Var (Var)
import ZkFold.Data.Binary (toByteString)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import Prelude (toInteger)

import ZkFold.Symbolic.Examples (ExampleOutput (..), examples)

fromBinary :: (Binary a, Ring b) => a -> b
fromBinary = foldr ((+) . fromConstant . toInteger) zero . toByteString

benchmark
  :: forall a i o
   . ( Arithmetic a
     , Binary a
     , NFData a
     , SymbolicInput i
     , HasRep i (Elem a)
     , Output (o (Elem a)) ~ o
     , SymbolicFunction (Elem a) (o (Elem a))
     , NFData (Layout o (Elem a) a)
     , NFData (Layout o (Elem a) (Var a))
     , Representable (Layout i (Elem a))
     )
  => String -> (i (Elem a) -> o (Elem a)) -> Benchmark
benchmark name fun =
  bgroup
    name
    [ bench "compilation" $ nf (compile @a id) fun
    , env (return $ force $ compile @a id fun) $ \c ->
        bench "evaluation" $ nf (`eval` tabulate fromBinary) c
    ]

main :: IO ()
main = defaultMain [benchmark n c | (n, ExampleOutput c) <- examples]
