{-# LANGUAGE TypeOperators #-}

module ZkFold.Base.Protocol.IVC.Predicate where

import           Data.Binary                (Binary)
import           GHC.Generics               (U1 (..), (:*:) (..))
import           Prelude                    hiding (Num (..), drop, head, replicate, take, zipWith)

import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Compiler   (ArithmeticCircuit, compileWith, guessOutput, hlmap)
import           ZkFold.Symbolic.Data.Class (LayoutFunctor)

type PredicateFunction a i p =
  forall c. (Symbolic c, BaseField c ~ a) => c i -> c p -> c i
type PredicateCircuit a i p  = ArithmeticCircuit a (i :*: p) i i

data Predicate a i p = Predicate
    { predicateFunction :: PredicateFunction a i p
    , predicateCircuit  :: PredicateCircuit a i p
    }

predicate ::
  (Arithmetic a, Binary a, LayoutFunctor i, LayoutFunctor p) =>
  PredicateFunction a i p -> Predicate a i p
predicate source = Predicate
    { predicateFunction = source
    , predicateCircuit =
        hlmap (U1 :*:) $
        compileWith
          guessOutput
          (\(i :*: p) U1 -> (U1 :*: U1 :*: U1, i :*: p :*: U1))
          source
    }
