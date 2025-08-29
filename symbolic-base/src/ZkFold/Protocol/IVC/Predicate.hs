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
import ZkFold.Symbolic.Data.Class (LayoutFunctor, RepLayout)
import ZkFold.Symbolic.Data.Vec (Vec (..))

data Predicate a i p = Predicate
  { predicateEval :: i a -> p a -> i a
  , predicateCircuit :: ArithmeticCircuit a (i :*: p :*: i) U1
  }

type StepFunction a i p =
  Vec i (CircuitContext a)
  -> Vec p (CircuitContext a)
  -> Vec i (CircuitContext a)

type Compilable f = (LayoutFunctor f, RepLayout f)

predicate
  :: forall a i p
   . (Arithmetic a, Binary a, Compilable i, Compilable p)
  => StepFunction a i p
  -> Predicate a i p
predicate func =
  Predicate
    { predicateEval = \x u ->
        exec . runVec . compileWith solder (const (U1, U1)) $
          func (Vec $ embed x) (Vec $ embed u)
    , predicateCircuit =
        runVec $ compileWith
          (guessOutput \(i :*: p :*: j) -> (i :*: p, j))
          (\(i :*: p) -> (U1 :*: U1 :*: U1, i :*: p :*: U1))
          func
    }
