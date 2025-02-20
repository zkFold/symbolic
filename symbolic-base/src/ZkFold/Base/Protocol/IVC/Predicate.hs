{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

module ZkFold.Base.Protocol.IVC.Predicate where

import           Data.Binary                       (Binary)
import           GHC.Generics                      (U1 (..), (:*:) (..))
import           Prelude                           hiding (Num (..), drop, head, replicate, take, zipWith)

import           ZkFold.Base.Algebra.Basic.Class   (FiniteField, FromConstant, Scale)
import           ZkFold.Base.Data.Package          (unpacked, packed)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Compiler          (ArithmeticCircuit, compileWith, guessOutput, hlmap)
import           ZkFold.Symbolic.Data.Class        (LayoutFunctor)
import           ZkFold.Symbolic.Data.FieldElement (FieldElement (..))

type PredicateFunctionAssumptions a f =
    ( FiniteField f
    , FromConstant a f
    , Scale a f
    )

type PredicateFunction a i p =
  forall c. (Symbolic c, BaseField c ~ a) => i (FieldElement c) -> p (FieldElement c) -> i (FieldElement c)
type PredicateCircuit a i p  = ArithmeticCircuit a (i :*: p) i i

data Predicate a i p = Predicate
    { predicateFunction :: PredicateFunction a i p
    , predicateCircuit  :: PredicateCircuit a i p
    }

predicate :: forall a i p .
    ( Arithmetic a
    , Binary a
    , LayoutFunctor i
    , LayoutFunctor p
    ) => PredicateFunction a i p -> Predicate a i p
predicate predicateFunction =
    let
        func' :: forall ctx .
            ( Symbolic ctx
            , BaseField ctx ~ a
            ) => ctx i -> ctx p -> ctx i
        func' x' u' =
            let
                x = FieldElement <$> unpacked x'
                u = FieldElement <$> unpacked u'
            in
                packed . fmap fromFieldElement $ predicateFunction x u

        predicateCircuit :: PredicateCircuit a i p
        predicateCircuit =
            hlmap (U1 :*:) $
            compileWith @a guessOutput (\(i :*: p) U1 -> (U1 :*: U1 :*: U1, i :*: p :*: U1)) func'
    in Predicate {..}
