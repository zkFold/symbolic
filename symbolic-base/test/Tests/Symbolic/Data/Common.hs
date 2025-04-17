{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Symbolic.Data.Common
  ( specConstantRoundtrip
  , specSymbolicData
  , specSymbolicFunction1
  , specSymbolicFunction1WithPar
  , specSymbolicFunction2
  ) where

import           Data.Binary                      (Binary)
import           Data.Eq                          (Eq)
import           Data.Function                    (const, ($), id)
import           Data.Typeable                    (Proxy (..))
import           GHC.Generics                     (U1 (..))
import           Prelude                          (String, type (~), (++))
import           Test.Hspec                       (Spec, describe)
import           Test.QuickCheck                  (Arbitrary (..), (===))
import           Tests.Symbolic.ArithmeticCircuit (it)
import           Text.Show                        (Show)

import           ZkFold.Symbolic.Class            (Symbolic (BaseField), embed)
import           ZkFold.Symbolic.Compiler         (ArithmeticCircuit, exec, compileWith)
import           ZkFold.Symbolic.Data.Class       (SymbolicData (..), SymbolicOutput)
import           ZkFold.Symbolic.Interpreter      (Interpreter (..))
import ZkFold.Base.Algebra.Basic.Class (ToConstant (..), FromConstant (..))
import ZkFold.Symbolic.Compiler.ArithmeticCircuit (checkClosedCircuit)

{-
  For all symbolic types we need to do the following:
  1. Check that `fromConstant` and `toConstant` are inverse functions.
  2. For all related functions, check that the circuit evaluation is equivalent to the function evaluation and 
     that the circuit's constraints are satisfied by the respective witness.
  3. Run all type-specific checks in the interpreter context.
-}

specConstantRoundtrip :: forall a x .
  ( Arbitrary (x (Interpreter a))
  , Arbitrary (Const (x (Interpreter a)))
  , Eq (x (Interpreter a))
  , Eq (Const (x (Interpreter a)))
  , Show (x (Interpreter a))
  , Show (Const (x (Interpreter a)))
  , FromConstant (Const (x (Interpreter a))) (x (Interpreter a))
  , ToConstant (x (Interpreter a)) 
  ) => String -> String -> Spec
specConstantRoundtrip symType hType = do
  it (symType ++ " embeds " ++ hType) $
    \(x :: x (Interpreter a)) -> fromConstant (toConstant x :: Const (x (Interpreter a))) === x
  it (hType ++ " embeds " ++ symType) $
    \(x :: Const (x (Interpreter a))) -> toConstant (fromConstant x :: x (Interpreter a)) === x

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

specSymbolicData :: forall a x .
  ( Show a
  , Arbitrary (x (Interpreter a))
  , Eq (x (Interpreter a))
  , Show (x (Interpreter a))
  , MatchingSymbolicOutput x (Interpreter a) (ArithmeticCircuit a U1)
  , MatchingSymbolicOutput x (ArithmeticCircuit a U1) (Interpreter a)
  ) => String -> Spec
specSymbolicData desc = describe desc $ do
  it "compiles correctly" $
    \(x :: x (Interpreter a)) -> fromCircuit (toCircuit @a x) === x
  it "satisfies constraints" $
    \(x :: x (Interpreter a)) -> checkClosedCircuit @a $ compileWith id (\U1 -> (U1, U1)) $
      toCircuit @a x

type SymbolicFunction1 x y = forall c . Symbolic c
  => x c -> y c

specSymbolicFunction1 :: forall a x y .
  ( Show a
  , Arbitrary (x (Interpreter a))
  , Eq (y (Interpreter a))
  , Show (x (Interpreter a))
  , Show (y (Interpreter a))
  , MatchingSymbolicOutput x (Interpreter a) (ArithmeticCircuit a U1)
  , MatchingSymbolicOutput y (ArithmeticCircuit a U1) (Interpreter a)
  ) => String -> SymbolicFunction1 x y -> Spec
specSymbolicFunction1 desc func = describe desc $ do
  it "evaluates correctly" $
    \x -> fromCircuit (func @(ArithmeticCircuit a U1) $ toCircuit x) === func x
  it "satisfies constraints" $
    \x -> checkClosedCircuit @a $ compileWith id (\U1 -> (U1, U1)) $
      func @(ArithmeticCircuit a U1) (toCircuit x)

specSymbolicFunction1WithPar :: forall par a x y .
  ( Show a
  , Arbitrary par
  , Arbitrary (x (Interpreter a))
  , Eq (y (Interpreter a))
  , Show par
  , Show (x (Interpreter a))
  , Show (y (Interpreter a))
  , MatchingSymbolicOutput x (Interpreter a) (ArithmeticCircuit a U1)
  , MatchingSymbolicOutput y (ArithmeticCircuit a U1) (Interpreter a)
  ) => String -> (par -> SymbolicFunction1 x y) -> Spec
specSymbolicFunction1WithPar desc func = describe desc $ do
  it "evaluates correctly" $
    \par x -> fromCircuit (func par @(ArithmeticCircuit a U1) $ toCircuit x) === func par x
  it "satisfies constraints" $
    \par x -> checkClosedCircuit @a $ compileWith id (\U1 -> (U1, U1)) $
      func par @(ArithmeticCircuit a U1) (toCircuit x)

type SymbolicFunction2 x y z = forall c . Symbolic c
  => x c -> y c -> z c

specSymbolicFunction2 :: forall a x y z .
  ( Show a
  , Arbitrary (x (Interpreter a))
  , Arbitrary (y (Interpreter a))
  , Eq (z (Interpreter a))
  , Show (x (Interpreter a))
  , Show (y (Interpreter a))
  , Show (z (Interpreter a))
  , MatchingSymbolicOutput x (Interpreter a) (ArithmeticCircuit a U1)
  , MatchingSymbolicOutput y (Interpreter a) (ArithmeticCircuit a U1)
  , MatchingSymbolicOutput z (ArithmeticCircuit a U1) (Interpreter a)
  ) => String -> SymbolicFunction2 x y z -> Spec
specSymbolicFunction2 desc func = describe desc $ do
  it "evaluates correctly" $
    \x y -> fromCircuit (func @(ArithmeticCircuit a U1) (toCircuit x) (toCircuit y)) === func x y
  it "satisfies constraints" $
    \x y -> checkClosedCircuit @a $ compileWith id (\U1 -> (U1, U1)) $
      func @(ArithmeticCircuit a U1) (toCircuit x) (toCircuit y)
