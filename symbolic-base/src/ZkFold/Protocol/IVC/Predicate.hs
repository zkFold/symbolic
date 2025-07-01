{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Protocol.IVC.Predicate where

import Data.Binary (Binary)
import Data.Function (const, ($), (.))
import GHC.Generics (U1 (..), (:*:) (..))

import ZkFold.ArithmeticCircuit (ArithmeticCircuit, exec, guessOutput, solder)
import ZkFold.ArithmeticCircuit.Context (CircuitContext)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Compiler (compileWith)
import ZkFold.Symbolic.Data.Class (LayoutFunctor)

data Predicate a i p = Predicate
  { predicateEval :: i a -> p a -> i a
  , predicateCircuit :: ArithmeticCircuit a (i :*: p :*: i) U1
  }

type StepFunction a i p =
  CircuitContext a i -> CircuitContext a p -> CircuitContext a i

predicate
  :: forall a i p
   . (Arithmetic a, Binary a, LayoutFunctor i, LayoutFunctor p)
  => StepFunction a i p
  -> Predicate a i p
predicate func =
  Predicate
    { predicateEval = \x u ->
        exec . compileWith solder (const (U1, U1)) $ func (embed x) (embed u)
    , predicateCircuit =
        compileWith
          (guessOutput \(i :*: p :*: j) -> (i :*: p, j))
          (\(i :*: p) -> (U1 :*: U1 :*: U1, i :*: p :*: U1))
          func
    }
