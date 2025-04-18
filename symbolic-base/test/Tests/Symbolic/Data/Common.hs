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

import           ZkFold.Base.Algebra.Basic.Class  (FromConstant (..), ToConstant (..))
import           ZkFold.Symbolic.Class            (Arithmetic, Symbolic (BaseField))
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

type MatchingSymbolicInput x i c c' =
  ( SymbolicInput (x c)
  , SymbolicInput (x c')
  , Context (x c) ~ c
  , Context (x c') ~ c'
  , Layout (x c) ~ Layout (x c')
  , Layout (x c) ~ i
  , Payload (x c) ~ Payload (x c')
  , Payload (x c) ~ U1
  , BaseField (Context (x c)) ~ BaseField (Context (x c'))
  , Binary (BaseField (Context (x c)))
  )

type MatchingSymbolicOutput y o c c' =
  ( SymbolicOutput (y c)
  , SymbolicOutput (y c')
  , Context (y c) ~ c
  , Context (y c') ~ c'
  , Layout (y c) ~ Layout (y c')
  , Layout (y c) ~ o
  , Payload (y c) ~ Payload (y c')
  , Payload (y c) ~ U1
  , BaseField (Context (y c)) ~ BaseField (Context (y c'))
  , Binary (BaseField (Context (y c)))
  )

evalCircuit0 :: forall a x o . (Arithmetic a, Binary a, Representable o
  , SymbolicInput (x (Interpreter a))
  , Context (x (Interpreter a)) ~ Interpreter a
  , Layout (x (Interpreter a)) ~ o
  , Payload (x (Interpreter a)) ~ U1
  ) => ArithmeticCircuit a U1 o -> x (Interpreter a)
evalCircuit0 ac =
  restore $ const (Interpreter $ exec ac, U1)

evalCircuit1 :: forall a x y i o . (Arithmetic a, Binary a, Representable i, Representable o
  , SymbolicInput (x (Interpreter a))
  , Context (x (Interpreter a)) ~ Interpreter a
  , Layout (x (Interpreter a)) ~ i
  , SymbolicOutput (y (Interpreter a))
  , Context (y (Interpreter a)) ~ Interpreter a
  , Layout (y (Interpreter a)) ~ o
  , Payload (y (Interpreter a)) ~ U1
  ) => ArithmeticCircuit a i o -> x (Interpreter a) -> y (Interpreter a)
evalCircuit1 ac input =
  restore $ const (Interpreter $ eval ac $ runInterpreter $ arithmetize input Proxy, U1)

evalCircuit2 :: forall a x y z ix iy i o . (Arithmetic a, Binary a, Representable i, Representable o
  , SymbolicInput (x (Interpreter a))
  , Context (x (Interpreter a)) ~ Interpreter a
  , Layout (x (Interpreter a)) ~ ix
  , SymbolicInput (y (Interpreter a))
  , Context (y (Interpreter a)) ~ Interpreter a
  , Layout (y (Interpreter a)) ~ iy
  , SymbolicOutput (z (Interpreter a))
  , Context (z (Interpreter a)) ~ Interpreter a
  , Layout (z (Interpreter a)) ~ o
  , Payload (z (Interpreter a)) ~ U1
  , i ~ ix :*: iy
  ) => ArithmeticCircuit a i o -> x (Interpreter a) -> y (Interpreter a) -> z (Interpreter a)
evalCircuit2 ac x y =
  restore $ const (Interpreter $ eval ac input, U1)
  where
    ix = runInterpreter $ arithmetize x Proxy
    iy = runInterpreter $ arithmetize y Proxy
    input = ix :*: iy

compileCircuit0 :: forall a x o . (Binary a
  , SymbolicInput (x (ArithmeticCircuit a U1))
  , Context (x (ArithmeticCircuit a U1)) ~ ArithmeticCircuit a U1
  , Layout (x (ArithmeticCircuit a U1)) ~ o
  , Payload (x (ArithmeticCircuit a U1)) ~ U1
  ) => (forall c . Symbolic c => x c) -> ArithmeticCircuit a U1 o
compileCircuit0 v =
  compileWith @a id (\U1 -> (U1, U1)) $ v @(ArithmeticCircuit a U1)

compileCircuit1 :: forall a x y i o . (Binary a, Representable i
  , SymbolicInput (x (ArithmeticCircuit a i))
  , Context (x (ArithmeticCircuit a i)) ~ ArithmeticCircuit a i
  , Layout (x (ArithmeticCircuit a i)) ~ i
  , Payload (x (ArithmeticCircuit a i)) ~ U1
  , SymbolicOutput (y (ArithmeticCircuit a i))
  , Context (y (ArithmeticCircuit a i)) ~ ArithmeticCircuit a i
  , Layout (y (ArithmeticCircuit a i)) ~ o
  ) => (forall c . Symbolic c => x c -> y c) -> ArithmeticCircuit a i o
compileCircuit1 func =
  compileWith @a id (\i -> (U1 :*: U1, i :*: U1)) $ func @(ArithmeticCircuit a i)

compileCircuit2 :: forall a x y z ix iy i o . (Binary a, Representable i
  , SymbolicInput (x (ArithmeticCircuit a i))
  , Context (x (ArithmeticCircuit a i)) ~ ArithmeticCircuit a i
  , Layout (x (ArithmeticCircuit a i)) ~ ix
  , Payload (x (ArithmeticCircuit a i)) ~ U1
  , SymbolicInput (y (ArithmeticCircuit a i))
  , Context (y (ArithmeticCircuit a i)) ~ ArithmeticCircuit a i
  , Layout (y (ArithmeticCircuit a i)) ~ iy
  , Payload (y (ArithmeticCircuit a i)) ~ U1
  , SymbolicOutput (z (ArithmeticCircuit a i))
  , Context (z (ArithmeticCircuit a i)) ~ ArithmeticCircuit a i
  , Layout (z (ArithmeticCircuit a i)) ~ o
  , i ~ ix :*: iy
  ) => (forall c . Symbolic c => x c -> y c -> z c) -> ArithmeticCircuit a i o
compileCircuit2 func =
  compileWith @a id (\(ix :*: iy) -> (U1 :*: U1 :*: U1, ix :*: iy :*: U1)) $ func @(ArithmeticCircuit a i)

type SymbolicFunction0 x = forall c . Symbolic c
  => x c

specSymbolicFunction0 :: forall a x o .
  ( Show a
  , Eq (x (Interpreter a))
  , Show (x (Interpreter a))
  , MatchingSymbolicInput x o (Interpreter a) (ArithmeticCircuit a U1)
  ) => String -> SymbolicFunction0 x -> Spec
specSymbolicFunction0 desc v = describe desc $ do
  it "evaluates correctly" $ evalCircuit0 @a (compileCircuit0 v) === v
  it "satisfies constraints" $
    checkClosedCircuit (compileCircuit0 @a v :: ArithmeticCircuit a U1 o)

type SymbolicFunction1 x y = forall c . Symbolic c
  => x c -> y c

specSymbolicFunction1 :: forall a x y i o .
  ( Show a
  , Arbitrary (x (Interpreter a))
  , Eq (y (Interpreter a))
  , Show (x (Interpreter a))
  , Show (y (Interpreter a))
  , MatchingSymbolicInput x i (Interpreter a) (ArithmeticCircuit a i)
  , MatchingSymbolicOutput y o (Interpreter a) (ArithmeticCircuit a i)
  ) => String -> SymbolicFunction1 x y -> Spec
specSymbolicFunction1 desc func = describe desc $ do
  it "evaluates correctly" $
    \(x :: x (Interpreter a)) -> evalCircuit1 @a (compileCircuit1 func) x === func x
  it "satisfies constraints" $
    checkCircuit (compileCircuit1 @a func :: ArithmeticCircuit a i o) (\(x :: x (Interpreter a)) -> runInterpreter $ arithmetize x Proxy)

specSymbolicFunction1WithPar :: forall par a x y i o .
  ( Show a
  , Arbitrary par
  , Arbitrary (x (Interpreter a))
  , Eq (y (Interpreter a))
  , Show par
  , Show (x (Interpreter a))
  , Show (y (Interpreter a))
  , MatchingSymbolicInput x i (Interpreter a) (ArithmeticCircuit a i)
  , MatchingSymbolicOutput y o (Interpreter a) (ArithmeticCircuit a i)
  ) => String -> (par -> SymbolicFunction1 x y) -> Spec
specSymbolicFunction1WithPar desc func = describe desc $ do
  it "evaluates correctly" $
    \par (x :: x (Interpreter a)) -> evalCircuit1 @a (compileCircuit1 $ func par) x === func par x
  it "satisfies constraints" $
    \par -> checkCircuit (compileCircuit1 @a $ func par :: ArithmeticCircuit a i o) (\(x :: x (Interpreter a)) -> runInterpreter $ arithmetize x Proxy)

type SymbolicFunction2 x y z = forall c . Symbolic c
  => x c -> y c -> z c

specSymbolicFunction2 :: forall a x y z ix iy i o.
  ( Show a
  , Arbitrary (x (Interpreter a))
  , Arbitrary (y (Interpreter a))
  , Eq (z (Interpreter a))
  , Show (x (Interpreter a))
  , Show (y (Interpreter a))
  , Show (z (Interpreter a))
  , MatchingSymbolicInput x ix (Interpreter a) (ArithmeticCircuit a i)
  , MatchingSymbolicInput y iy (Interpreter a) (ArithmeticCircuit a i)
  , MatchingSymbolicOutput z o (Interpreter a) (ArithmeticCircuit a i)
  , i ~ ix :*: iy
  ) => String -> SymbolicFunction2 x y z -> Spec
specSymbolicFunction2 desc func = describe desc $ do
  it "evaluates correctly" $
    \(x :: x (Interpreter a)) (y :: y (Interpreter a)) ->
      evalCircuit2 (compileCircuit2 func) x y === func x y
  it "satisfies constraints" $
    checkCircuit (compileCircuit2 @a func :: ArithmeticCircuit a i o)
      (\((x, y) :: (x (Interpreter a), y (Interpreter a))) -> runInterpreter (arithmetize x Proxy) :*: runInterpreter (arithmetize y Proxy))
