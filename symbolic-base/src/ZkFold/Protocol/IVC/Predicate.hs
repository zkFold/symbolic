{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators  #-}

module ZkFold.Protocol.IVC.Predicate where

import           Data.Binary                                (Binary)
import           GHC.Generics                               (U1 (..), (:*:) (..))
import           Prelude                                    hiding (Num (..), drop, head, replicate, take, zipWith)

import           ZkFold.Data.Package                        (packed, unpacked)
import           ZkFold.Protocol.IVC.StepFunction           (StepFunction, StepFunctionAssumptions)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Compiler                   (compileWith)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit (ArithmeticCircuit, guessOutput)
import           ZkFold.Symbolic.Data.Class                 (LayoutFunctor)
import           ZkFold.Symbolic.Data.FieldElement          (FieldElement (..))
import           ZkFold.Symbolic.Interpreter                (Interpreter (..))

type PredicateCircuit a i p = ArithmeticCircuit a (i :*: p :*: i) U1

data Predicate a i p = Predicate
    { predicateEval    :: i a -> p a -> i a
    , predicateCircuit :: PredicateCircuit a i p
    }

type PredicateAssumptions a i p =
    ( Arithmetic a
    , Binary a
    , LayoutFunctor i
    , LayoutFunctor p
    )

predicate :: forall a i p . PredicateAssumptions a i p
    => StepFunction a i p -> Predicate a i p
predicate func =
    let
        func' :: forall f ctx . StepFunctionAssumptions a f ctx => ctx i -> ctx p -> ctx i
        func' x' u' =
            let
                x = FieldElement <$> unpacked x'
                u = FieldElement <$> unpacked u'
            in
                packed . fmap fromFieldElement $ func x u

        predicateEval :: i a -> p a -> i a
        predicateEval x u = runInterpreter $ func' (Interpreter x) (Interpreter u)

        predicateCircuit :: PredicateCircuit a i p
        predicateCircuit =
            compileWith (guessOutput \(i :*: p :*: j) -> (i :*: p, j))
                        (\(i :*: p) -> (U1 :*: U1 :*: U1, i :*: p :*: U1)) func'
    in Predicate {..}
