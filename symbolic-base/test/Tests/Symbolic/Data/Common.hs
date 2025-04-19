{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Symbolic.Data.Common
  ( specConstantRoundtrip
  , specSymbolicFunction0
  , specSymbolicFunction1
  , specSymbolicFunction1WithPar
  , specSymbolicFunction2
  ) where

import           Data.Binary                      (Binary)
import           Data.Eq                          (Eq)
import           Data.Function                    (const, id, ($))
import           Data.Functor.Rep                 (Representable (..))
import           Data.Typeable                    (Proxy (..))
import           GHC.Generics                     (U1 (..), type (:*:) (..))
import           Prelude                          (String, return, type (~), (++))
import           Test.Hspec                       (Spec, describe)
import           Test.QuickCheck                  (Arbitrary (..), Gen, (===))
import           Tests.Symbolic.ArithmeticCircuit (it)
import           Text.Show                        (Show)

import           ZkFold.Algebra.Class             (FromConstant (..), ToConstant (..))
import           ZkFold.Symbolic.Class            (Arithmetic, Symbolic)
import           ZkFold.Symbolic.Compiler         (ArithmeticCircuit, checkCircuit, checkClosedCircuit, compileWith,
                           eval, exec)
import           ZkFold.Symbolic.Data.Class       (SymbolicData (..), SymbolicOutput)
import           ZkFold.Symbolic.Data.Input       (SymbolicInput)
import           ZkFold.Symbolic.Interpreter      (Interpreter (..))

{-
  For all symbolic types we need to do the following:
  1. Check that `fromConstant` and `toConstant` are inverse functions.
  2. For all related functions, check that the circuit evaluation is equivalent to the function evaluation and
     that the circuit's constraints are satisfied by the respective witness.
  3. Run all type-specific checks in the interpreter context.
-}

specConstantRoundtrip' :: forall x .
  ( Arbitrary x
  , Eq x
  , Eq (Const x)
  , Show x
  , Show (Const x)
  , FromConstant (Const x) x
  , ToConstant x
  ) => String -> String -> Gen (Const x) -> Spec
specConstantRoundtrip' symType hType gen = do
  it (symType ++ " embeds " ++ hType) $
    \(x :: x) -> fromConstant (toConstant x :: Const x) === x
  it (hType ++ " embeds " ++ symType) $ do
    (x :: Const x) <- gen
    return $ toConstant (fromConstant x :: x) === x

specConstantRoundtrip :: forall a x .
  ( Arbitrary (x (Interpreter a))
  , Eq (x (Interpreter a))
  , Eq (Const (x (Interpreter a)))
  , Show (x (Interpreter a))
  , Show (Const (x (Interpreter a)))
  , FromConstant (Const (x (Interpreter a))) (x (Interpreter a))
  , ToConstant (x (Interpreter a))
  ) => String -> String -> Gen (Const (x (Interpreter a))) -> Spec
specConstantRoundtrip = specConstantRoundtrip' @(x (Interpreter a))

type Match cls a i l x x' =
    ( cls x, cls x', Context x ~ Interpreter a, Context x' ~ ArithmeticCircuit a i
    , Support x ~ Proxy (Context x), Support x' ~ Proxy (Context x')
    , Layout x ~ Layout x', Layout x ~ l, Payload x ~ Payload x', Payload x ~ U1
    , Show x, Binary a, Show a )
type Matching constr a i l x = Match constr a i l (x (Interpreter a)) (x (ArithmeticCircuit a i))
type MatchingSymbolicInput a i l x = (Matching SymbolicInput a i l x, Arbitrary (x (Interpreter a)))
type MatchingSymbolicOutput a i l x = (Matching SymbolicData a i l x, Eq (x (Interpreter a)))

evalCircuit0 :: forall x a o .
  ( Arithmetic a
  , Binary a
  , Representable o
  , SymbolicInput x
  , Context x ~ Interpreter a
  , Layout x ~ o
  , Payload x ~ U1
  ) => ArithmeticCircuit a U1 o -> x
evalCircuit0 ac =
  restore $ const (Interpreter $ exec ac, U1)

evalCircuit1 :: forall x y a o .
  ( Arithmetic a
  , Binary a
  , Representable o
  , SymbolicInput x
  , Context x ~ Interpreter a
  , SymbolicOutput y
  , Context y ~ Interpreter a
  , Layout y ~ o
  , Payload y ~ U1
  ) => ArithmeticCircuit a (Layout x) o -> x -> y
evalCircuit1 ac input =
  restore $ const (Interpreter $ eval ac $ runInterpreter $ arithmetize input Proxy, U1)

evalCircuit2 :: forall x y z a o .
  ( Arithmetic a
  , Binary a
  , Representable o
  , SymbolicInput x
  , Context x ~ Interpreter a
  , SymbolicInput y
  , Context y ~ Interpreter a
  , SymbolicOutput z
  , Context z ~ Interpreter a
  , Layout z ~ o
  , Payload z ~ U1
  ) => ArithmeticCircuit a (Layout x :*: Layout y) o -> x -> y -> z
evalCircuit2 ac x y =
  restore $ const (Interpreter $ eval ac input, U1)
  where
    ix = runInterpreter $ arithmetize x Proxy
    iy = runInterpreter $ arithmetize y Proxy
    input = ix :*: iy

compileCircuit0 :: forall x a o .
  ( Binary a
  , SymbolicInput (x (ArithmeticCircuit a U1))
  , Context (x (ArithmeticCircuit a U1)) ~ ArithmeticCircuit a U1
  , Layout (x (ArithmeticCircuit a U1)) ~ o
  , Payload (x (ArithmeticCircuit a U1)) ~ U1
  ) => (forall c . Symbolic c => x c) -> ArithmeticCircuit a U1 o
compileCircuit0 v =
  compileWith @a id (\U1 -> (U1, U1)) $ v @(ArithmeticCircuit a U1)

compileCircuit1 :: forall x y a i o .
  ( Binary a
  , Representable i
  , SymbolicInput x
  , Context x ~ ArithmeticCircuit a i
  , Layout x ~ i
  , Payload x ~ U1
  , SymbolicOutput y
  , Context y ~ ArithmeticCircuit a i
  , Layout y ~ o
  ) => (x -> y) -> ArithmeticCircuit a i o
compileCircuit1 func =
  compileWith @a id (\i -> (U1 :*: U1, i :*: U1)) func

compileCircuit2 :: forall x y z a i o .
  ( Binary a
  , Representable i
  , SymbolicInput x
  , Context x ~ ArithmeticCircuit a i
  , Payload x ~ U1
  , SymbolicInput y
  , Context y ~ ArithmeticCircuit a i
  , Payload y ~ U1
  , SymbolicOutput z
  , Context z ~ ArithmeticCircuit a i
  , Layout z ~ o
  , i ~ Layout x :*: Layout y
  ) => (x -> y -> z) -> ArithmeticCircuit a i o
compileCircuit2 func =
  compileWith @a id (\(ix :*: iy) -> (U1 :*: U1 :*: U1, ix :*: iy :*: U1)) func

type SymbolicFunction0 x = forall c . Symbolic c
  => x c

specSymbolicFunction0 :: forall a x o .
  ( MatchingSymbolicInput a U1 o x
  , MatchingSymbolicOutput a U1 o x
  ) =>String -> SymbolicFunction0 x -> Spec
specSymbolicFunction0 desc v = describe desc $ do
  it "evaluates correctly" $ evalCircuit0 @(x (Interpreter a)) (compileCircuit0 v) === v
  it "satisfies constraints" $
    checkClosedCircuit (compileCircuit0 v :: ArithmeticCircuit a U1 o)

type SymbolicFunction1 x y = forall c . Symbolic c
  => x c -> y c

specSymbolicFunction1 :: forall a x y i o .
  ( MatchingSymbolicInput a i i x
  , MatchingSymbolicOutput a i o y
  ) => String -> SymbolicFunction1 x y -> Spec
specSymbolicFunction1 desc func = describe desc $ do
  it "evaluates correctly" $
    \(x :: x (Interpreter a)) ->
      evalCircuit1 (compileCircuit1 $ func @(ArithmeticCircuit a i)) x === func x
  it "satisfies constraints" $
    checkCircuit (compileCircuit1 $ func @(ArithmeticCircuit a i)) (\(x :: x (Interpreter a)) -> runInterpreter $ arithmetize x Proxy)

specSymbolicFunction1WithPar :: forall par a x y i o .
  ( Arbitrary par
  , Show par
  , MatchingSymbolicInput a i i x
  , MatchingSymbolicOutput a i o y
  ) => String -> (par -> SymbolicFunction1 x y) -> Spec
specSymbolicFunction1WithPar desc func = describe desc $ do
  it "evaluates correctly" $
    \par (x :: x (Interpreter a)) ->
      evalCircuit1 (compileCircuit1 $ func par @(ArithmeticCircuit a i)) x === func par x
  it "satisfies constraints" $
    \par -> checkCircuit (compileCircuit1 $ func par @(ArithmeticCircuit a i)) (\(x :: x (Interpreter a)) -> runInterpreter $ arithmetize x Proxy)

type SymbolicFunction2 x y z = forall c . Symbolic c
  => x c -> y c -> z c

specSymbolicFunction2 :: forall a x y z ix iy i o.
  ( MatchingSymbolicInput a i ix x
  , MatchingSymbolicInput a i iy y
  , MatchingSymbolicOutput a i o z
  , i ~ ix :*: iy
  ) => String -> SymbolicFunction2 x y z -> Spec
specSymbolicFunction2 desc func = describe desc $ do
  it "evaluates correctly" $
    \(x :: x (Interpreter a)) (y :: y (Interpreter a)) ->
      evalCircuit2 (compileCircuit2 $ func @(ArithmeticCircuit a i)) x y === func x y
  it "satisfies constraints" $
    checkCircuit (compileCircuit2 $ func @(ArithmeticCircuit a i))
      (\((x, y) :: (x (Interpreter a), y (Interpreter a))) -> runInterpreter (arithmetize x Proxy) :*: runInterpreter (arithmetize y Proxy))
