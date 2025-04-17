{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Symbolic.Data.Common (specSymbolicFunction1, specSymbolicFunction2) where

import           Data.Binary                      (Binary)
import           Data.Eq                          (Eq)
import           Data.Function                    (const, ($), id)
import           Data.Typeable                    (Proxy (..))
import           GHC.Generics                     (U1 (..))
import           Prelude                          (String, type (~))
import           Test.Hspec                       (Spec)
import           Test.QuickCheck                  (Arbitrary (..), (===))
import           Tests.Symbolic.ArithmeticCircuit (it)
import           Text.Show                        (Show)

import           ZkFold.Symbolic.Class            (Symbolic (BaseField), embed)
import           ZkFold.Symbolic.Compiler         (ArithmeticCircuit, exec, compileWith)
import           ZkFold.Symbolic.Data.Class       (SymbolicData (..), SymbolicOutput)
import           ZkFold.Symbolic.Interpreter      (Interpreter (..))

{-
  For all symbolic types we need to do the following:
  1. Check that `fromConstant` and `toConstant` are inverse functions.
  2. Run all type-specific checks in the interpreter context.
  3. For all related functions, check that the circuit evaluation is equivalent to the function evaluation.
  4. For all related functions, Check that the circuit's constraints are satisfied by the respective witness.
-}

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

-- TODO: Use `eval` instead of `exec`

fromCircuit :: forall a x . (MatchingSymbolicOutput x (ArithmeticCircuit a U1) (Interpreter a))
  => x (ArithmeticCircuit a U1) -> x (Interpreter a)
fromCircuit x =
  restore $ const (Interpreter $ exec $ compileWith id (\U1 -> (U1, U1)) x, U1)

toCircuit :: forall a x . MatchingSymbolicOutput x (Interpreter a) (ArithmeticCircuit a U1)
  => x (Interpreter a) -> x (ArithmeticCircuit a U1)
toCircuit x =
  restore $ const (embed $ runInterpreter $ arithmetize x Proxy, U1)

type SymbolicFunction1 x y = forall c . Symbolic c
  => x c -> y c

specSymbolicFunction1 :: forall a x y .
  ( Arbitrary (x (Interpreter a))
  , Eq (y (Interpreter a))
  , Show (x (Interpreter a))
  , Show (y (Interpreter a))
  , MatchingSymbolicOutput x (Interpreter a) (ArithmeticCircuit a U1)
  , MatchingSymbolicOutput y (ArithmeticCircuit a U1) (Interpreter a)
  ) => String -> SymbolicFunction1 x y -> Spec
specSymbolicFunction1 desc func = it desc $
  \x -> fromCircuit (func @(ArithmeticCircuit a U1) $ toCircuit x) === func x

type SymbolicFunction2 x y z = forall c . Symbolic c
  => x c -> y c -> z c

specSymbolicFunction2 :: forall a x y z .
  ( Arbitrary (x (Interpreter a))
  , Arbitrary (y (Interpreter a))
  , Eq (z (Interpreter a))
  , Show (x (Interpreter a))
  , Show (y (Interpreter a))
  , Show (z (Interpreter a))
  , MatchingSymbolicOutput x (Interpreter a) (ArithmeticCircuit a U1)
  , MatchingSymbolicOutput y (Interpreter a) (ArithmeticCircuit a U1)
  , MatchingSymbolicOutput z (ArithmeticCircuit a U1) (Interpreter a)
  ) => String -> SymbolicFunction2 x y z -> Spec
specSymbolicFunction2 desc func = it desc $
  \x y -> fromCircuit (func @(ArithmeticCircuit a U1) (toCircuit x) (toCircuit y)) === func x y