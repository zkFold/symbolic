{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Symbolic.Data.Common (specSymbolicData) where

import           Data.Binary                      (Binary)
import           Data.Eq                          (Eq)
import           Data.Function                    (const, ($))
import           Data.Typeable                    (Proxy (..))
import           GHC.Generics                     (U1 (..))
import           Prelude                          (String, type (~))
import           Test.Hspec                       (Spec)
import           Test.QuickCheck                  (Arbitrary (..), Property, (===))
import           Tests.Symbolic.ArithmeticCircuit (it)
import           Text.Show                        (Show)

import           ZkFold.Symbolic.Class            (Symbolic (BaseField), embed)
import           ZkFold.Symbolic.Compiler         (ArithmeticCircuit, exec)
import           ZkFold.Symbolic.Data.Class       (SymbolicData (..), SymbolicOutput)
import           ZkFold.Symbolic.Interpreter      (Interpreter (..))

{-
  For all symbolic types we need to do the following:
  1. Check that `fromConstant` and `toConstant` are inverse functions.
  2. Run all type-specific checks in the interpreter context.
  3. For all related functions, check that the circuit evaluation is equivalent to the function evaluation.
  4. For all related functions, Check that the circuit's constraints are satisfied by the respective witness.
-}

type Func1 x y = x -> y
type Predicate x = x -> Property

type SymbolicFunction x y = forall c . Symbolic c
  => x c -> y c

type MatchingSymbolicOutput x c c' =
  ( SymbolicOutput (x c)
  , SymbolicOutput (x c')
  , Context (x c) ~ c
  , Context (x c') ~ c'
  , Layout (x c) ~ Layout (x c')
  , Payload (x c) ~ Payload (x c')
  , Payload (x c) ~ U1
  , BaseField (Context (x c)) ~ BaseField (Context (x c'))
  , Binary (BaseField (Context (x c)))
  )

-- TODO: this does not properly compile a circuit!

fromCircuit :: forall a x . MatchingSymbolicOutput x (ArithmeticCircuit a U1) (Interpreter a)
  => x (ArithmeticCircuit a U1) -> x (Interpreter a)
fromCircuit x =
  restore $ const (Interpreter $ exec $ arithmetize x Proxy, U1)

toCircuit :: forall a x . MatchingSymbolicOutput x (Interpreter a) (ArithmeticCircuit a U1)
  => x (Interpreter a) -> x (ArithmeticCircuit a U1)
toCircuit x =
  restore $ const (embed $ runInterpreter $ arithmetize x Proxy, U1)

evaluatesCorrectly :: forall a x y.
  ( MatchingSymbolicOutput x (Interpreter a) (ArithmeticCircuit a U1)
  , MatchingSymbolicOutput y (ArithmeticCircuit a U1) (Interpreter a)
  , Eq (y (Interpreter a))
  , Show (y (Interpreter a))
  ) => Func1 (x (Interpreter a)) (y (Interpreter a))
  -> Func1 (x (ArithmeticCircuit a U1)) (y (ArithmeticCircuit a U1))
  -> Predicate (x (Interpreter a))
evaluatesCorrectly f g x = fromCircuit (g $ toCircuit x) === (f x)

specSymbolicData :: forall a x y .
  ( Arbitrary (x (Interpreter a))
  , Eq (y (Interpreter a))
  , Show (x (Interpreter a))
  , Show (y (Interpreter a))
  , MatchingSymbolicOutput x (Interpreter a) (ArithmeticCircuit a U1)
  , MatchingSymbolicOutput y (ArithmeticCircuit a U1) (Interpreter a)
  ) => String -> SymbolicFunction x y -> Spec
specSymbolicData desc fun = it desc $
  evaluatesCorrectly (fun @(Interpreter a)) (fun @(ArithmeticCircuit a U1))
