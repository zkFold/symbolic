{-# LANGUAGE TypeOperators #-}

module Main where

import Control.DeepSeq (NFData1, force)
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
import ZkFold.ArithmeticCircuit.Elem (Elem, compileV2)
import ZkFold.ArithmeticCircuit.Node (Output, SymbolicFunction)
import ZkFold.Data.Binary (toByteString)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Data.V2 (SymbolicData (..))
import Prelude (toInteger)

import ZkFold.Symbolic.Examples (ExampleOutput (..), examples)

fromBinary :: (Binary a, Ring b) => a -> b
fromBinary = foldr ((+) . fromConstant . toInteger) zero . toByteString

benchmark
  :: forall a i o
   . ( Arithmetic a
     , Binary a
     , SymbolicData i
     , HasRep i (Elem a)
     , Output (o (Elem a)) ~ o
     , SymbolicFunction (Elem a) (o (Elem a))
     , NFData1 (Layout o (Elem a))
     , Representable (Layout i (Elem a))
     )
  => String -> (i (Elem a) -> o (Elem a)) -> Benchmark
benchmark name fun =
  bgroup
    name
    [ bench "compilation" $ nf (compileV2 @a id) fun
    , env (return $ force $ compileV2 @a id fun) $ \c ->
        bench "evaluation" $ nf (`eval` tabulate fromBinary) c
    ]

main :: IO ()
main = defaultMain [benchmark n c | (n, ExampleOutput c) <- examples]
