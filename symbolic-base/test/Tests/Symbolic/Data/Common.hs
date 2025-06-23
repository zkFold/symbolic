{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Symbolic.Data.Common (
  specConstantRoundtrip,
  specSymbolicFunction0,
  specSymbolicFunction1,
  specSymbolicFunction1WithPar,
  specSymbolicFunction2,
) where

import Data.Binary (Binary)
import Data.Eq (Eq)
import Data.Function (const, ($))
import Data.Typeable (Proxy (..))
import GHC.Generics (U1 (..), type (:*:) (..))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Arbitrary (..), Gen, (===))
import Text.Show (Show)
import Prelude (String, return, (++), type (~))

import Tests.Common (it)
import ZkFold.Algebra.Class (FromConstant (..), ToConstant (..))
import ZkFold.Symbolic.Class (Arithmetic, Symbolic)
import ZkFold.Symbolic.Compiler (compileWith)
import ZkFold.Symbolic.Compiler.ArithmeticCircuit (
  ArithmeticCircuit,
  checkCircuit,
  checkClosedCircuit,
  eval,
  exec,
  solder,
 )
import ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context (CircuitContext)
import ZkFold.Symbolic.Data.Class (SymbolicData (..), SymbolicOutput)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Interpreter (Interpreter (..))

{-
  For all symbolic types we need to do the following:
  1. Check that `fromConstant` and `toConstant` are inverse functions.
  2. For all related functions, check that the circuit evaluation is equivalent to the function evaluation and
     that the circuit's constraints are satisfied by the respective witness.
  3. Run all type-specific checks in the interpreter context.
-}

specConstantRoundtrip'
  :: forall x
   . ( Arbitrary x
     , Eq (Const x)
     , Eq x
     , FromConstant (Const x) x
     , Show (Const x)
     , Show x
     , ToConstant x
     )
  => String -> String -> Gen (Const x) -> Spec
specConstantRoundtrip' symType hType gen = do
  it (symType ++ " embeds " ++ hType) $
    \(x :: x) -> fromConstant (toConstant x :: Const x) === x
  it (hType ++ " embeds " ++ symType) $ do
    (x :: Const x) <- gen
    return $ toConstant (fromConstant x :: x) === x

specConstantRoundtrip
  :: forall a x
   . ( Arbitrary (x (Interpreter a))
     , Eq (Const (x (Interpreter a)))
     , Eq (x (Interpreter a))
     , FromConstant (Const (x (Interpreter a))) (x (Interpreter a))
     , Show (Const (x (Interpreter a)))
     , Show (x (Interpreter a))
     , ToConstant (x (Interpreter a))
     )
  => String -> String -> Gen (Const (x (Interpreter a))) -> Spec
specConstantRoundtrip = specConstantRoundtrip' @(x (Interpreter a))

type Match cls a x x' =
  ( cls x
  , cls x'
  , Context x ~ Interpreter a
  , Context x' ~ CircuitContext a
  , Support x ~ Proxy (Context x)
  , Support x' ~ Proxy (Context x')
  , Layout x ~ Layout x'
  , Payload x ~ Payload x'
  , Payload x ~ U1
  , Show x
  , Binary a
  , Show a
  )

type Matching constr a x = Match constr a (x (Interpreter a)) (x (CircuitContext a))

type MatchingSymbolicInput a x = (Matching SymbolicInput a x, Arbitrary (x (Interpreter a)))

type MatchingSymbolicOutput a x = (Matching SymbolicData a x, Eq (x (Interpreter a)))

evalCircuit0
  :: forall x a
   . ( Arithmetic a
     , Context x ~ Interpreter a
     , Payload x ~ U1
     , SymbolicInput x
     )
  => ArithmeticCircuit a U1 (Layout x) -> x
evalCircuit0 ac =
  restore $ const (Interpreter $ exec ac, U1)

evalCircuit1
  :: forall x y a
   . ( Arithmetic a
     , Context x ~ Interpreter a
     , Context y ~ Interpreter a
     , Payload y ~ U1
     , SymbolicInput x
     , SymbolicOutput y
     )
  => ArithmeticCircuit a (Layout x) (Layout y) -> x -> y
evalCircuit1 ac input =
  restore $ const (Interpreter $ eval ac $ runInterpreter $ arithmetize input Proxy, U1)

evalCircuit2
  :: forall x y z a
   . ( Arithmetic a
     , Context x ~ Interpreter a
     , Context y ~ Interpreter a
     , Context z ~ Interpreter a
     , Payload z ~ U1
     , SymbolicInput x
     , SymbolicInput y
     , SymbolicOutput z
     )
  => ArithmeticCircuit a (Layout x :*: Layout y) (Layout z) -> x -> y -> z
evalCircuit2 ac x y =
  restore $ const (Interpreter $ eval ac input, U1)
 where
  ix = runInterpreter $ arithmetize x Proxy
  iy = runInterpreter $ arithmetize y Proxy
  input = ix :*: iy

compileCircuit0
  :: forall x a o
   . ( Binary a
     , Context x ~ CircuitContext a
     , Layout x ~ o
     , Payload x ~ U1
     , SymbolicInput x
     )
  => x -> ArithmeticCircuit a U1 (Layout x)
compileCircuit0 = compileWith @a solder (\U1 -> (U1, U1))

compileCircuit1
  :: forall x y a
   . ( Binary a
     , Context x ~ CircuitContext a
     , Context y ~ CircuitContext a
     , Payload x ~ U1
     , SymbolicInput x
     , SymbolicOutput y
     )
  => (x -> y) -> ArithmeticCircuit a (Layout x) (Layout y)
compileCircuit1 = compileWith @a solder (\i -> (U1 :*: U1, i :*: U1))

compileCircuit2
  :: forall x y z a
   . ( Binary a
     , Context x ~ CircuitContext a
     , Context y ~ CircuitContext a
     , Context z ~ CircuitContext a
     , Payload x ~ U1
     , Payload y ~ U1
     , SymbolicInput x
     , SymbolicInput y
     , SymbolicOutput z
     )
  => (x -> y -> z) -> ArithmeticCircuit a (Layout x :*: Layout y) (Layout z)
compileCircuit2 =
  compileWith @a solder (\(ix :*: iy) -> (U1 :*: U1 :*: U1, ix :*: iy :*: U1))

type SymbolicFunction0 x =
  forall c
   . Symbolic c
  => x c

specSymbolicFunction0
  :: forall a x
   . ( MatchingSymbolicInput a x
     , MatchingSymbolicOutput a x
     )
  => String -> SymbolicFunction0 x -> Spec
specSymbolicFunction0 desc v = describe desc $ do
  it "evaluates correctly" $
    evalCircuit0 @(x (Interpreter a)) (compileCircuit0 $ v @(CircuitContext a)) === v
  it "satisfies constraints" $
    checkClosedCircuit (compileCircuit0 $ v @(CircuitContext a))

type SymbolicFunction1 x y =
  forall c
   . Symbolic c
  => x c -> y c

specSymbolicFunction1
  :: forall a x y
   . ( MatchingSymbolicInput a x
     , MatchingSymbolicOutput a y
     )
  => String -> SymbolicFunction1 x y -> Spec
specSymbolicFunction1 desc func = describe desc $ do
  it "evaluates correctly" $
    \(x :: x (Interpreter a)) ->
      evalCircuit1 (compileCircuit1 $ func @(CircuitContext a)) x === func x
  it "satisfies constraints" $
    checkCircuit
      (compileCircuit1 $ func @(CircuitContext a))
      (\(x :: x (Interpreter a)) -> runInterpreter $ arithmetize x Proxy)

specSymbolicFunction1WithPar
  :: forall par a x y
   . ( Arbitrary par
     , MatchingSymbolicInput a x
     , MatchingSymbolicOutput a y
     , Show par
     )
  => String -> (par -> SymbolicFunction1 x y) -> Spec
specSymbolicFunction1WithPar desc func = describe desc $ do
  it "evaluates correctly" $
    \par (x :: x (Interpreter a)) ->
      evalCircuit1 (compileCircuit1 $ func par @(CircuitContext a)) x === func par x
  it "satisfies constraints" $
    \par ->
      checkCircuit
        (compileCircuit1 $ func par @(CircuitContext a))
        (\(x :: x (Interpreter a)) -> runInterpreter $ arithmetize x Proxy)

type SymbolicFunction2 x y z =
  forall c
   . Symbolic c
  => x c -> y c -> z c

specSymbolicFunction2
  :: forall a x y z
   . ( MatchingSymbolicInput a x
     , MatchingSymbolicInput a y
     , MatchingSymbolicOutput a z
     )
  => String -> SymbolicFunction2 x y z -> Spec
specSymbolicFunction2 desc func = describe desc $ do
  it "evaluates correctly" $
    \(x :: x (Interpreter a)) (y :: y (Interpreter a)) ->
      evalCircuit2 (compileCircuit2 $ func @(CircuitContext a)) x y === func x y
  it "satisfies constraints" $
    checkCircuit
      (compileCircuit2 $ func @(CircuitContext a))
      ( \((x, y) :: (x (Interpreter a), y (Interpreter a))) -> runInterpreter (arithmetize x Proxy) :*: runInterpreter (arithmetize y Proxy)
      )
