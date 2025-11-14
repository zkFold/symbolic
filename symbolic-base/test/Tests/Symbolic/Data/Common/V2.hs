{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}

module Tests.Symbolic.Data.Common.V2
  ( specConstantRoundtripV2
  , specSymbolicFunction0V2
  , specSymbolicFunction1V2
  , specSymbolicFunction2V2
  ) where

import Data.Eq (Eq)
import Data.String (String)
import Data.Type.Equality (type (~))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Arbitrary, Gen, (===))
import Text.Show (Show)

import Tests.Symbolic.Data.Common (specConstantRoundtrip')
import ZkFold.Algebra.Class
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.V2 (Symbolic)
import ZkFold.Symbolic.Data.V2 (SymbolicData (..), RepData)
import ZkFold.ArithmeticCircuit.Elem (Elem, compileV2)
import Data.Binary (Binary)
import Data.Function (($), id)
import ZkFold.ArithmeticCircuit (exec, checkClosedCircuit, eval, checkCircuit)
import GHC.Generics (U1 (..), type (:*:) (..))
import ZkFold.ArithmeticCircuit.Node (Output, Input, SymbolicFunction)
import Tests.Common (it)

specConstantRoundtripV2
  :: forall a x
   . ( Arbitrary (x a)
     , Eq (x a)
     , Eq (Const (x a))
     , Show (x a)
     , Show (Const (x a))
     , FromConstant (Const (x a)) (x a)
     , ToConstant (x a)
     )
  => String
  -> String
  -> Gen (Const (x a))
  -> Spec
specConstantRoundtripV2 = specConstantRoundtrip' @(x a)

type Match a x = (Show (x a), Binary a, Show a)

type MatchingSymbolicInput a x =
  ( SymbolicData x
  , Match a x
  , HasRep x (Elem a)
  , HasRep x a
  , RepData x (Elem a)
  , Arbitrary (x a)
  )

type MatchingSymbolicOutput a x =
  ( SymbolicData x
  , Match a x
  , Eq (x a)
  , Input (x (Elem a)) ~ U1
  , Output (x (Elem a)) ~ x
  , SymbolicFunction (Elem a) (x (Elem a))
  )

specSymbolicFunction0V2
  :: forall a x
   . (Arithmetic a, MatchingSymbolicInput a x, MatchingSymbolicOutput a x)
  => String -> (forall c. Symbolic c => x c) -> Spec
specSymbolicFunction0V2 desc v = describe desc do
  it "evaluates correctly" $
    fromLayout (exec $ compileV2 id $ v @(Elem a)) === v
  it "satisfies constraints" $
    checkClosedCircuit (compileV2 id $ v @(Elem a))

specSymbolicFunction1V2
  :: forall a x y
   . (Arithmetic a, MatchingSymbolicInput a x, MatchingSymbolicOutput a y)
  => String -> (forall c. Symbolic c => x c -> y c) -> Spec
specSymbolicFunction1V2 desc func = describe desc do
  it "evaluates correctly" \(x :: x a) ->
    fromLayout (eval (compileV2 (:*: U1) $ func @(Elem a)) $ toLayout x)
    === func x
  it "satisfies constraints" $
    checkCircuit
      (compileV2 (:*: U1) $ func @(Elem a))
      (\(x :: x a) -> toLayout x)

specSymbolicFunction2V2
  :: forall a x y z
   . ( Arithmetic a
     , MatchingSymbolicInput a x
     , MatchingSymbolicInput a y
     , MatchingSymbolicOutput a z
     )
  => String -> (forall c. Symbolic c => x c -> y c -> z c) -> Spec
specSymbolicFunction2V2 desc func = describe desc do
  it "evaluates correctly" \(x :: x a) (y :: y a) ->
    fromLayout
      (eval (compileV2 id $ func @(Elem a)) $ toLayout x :*: toLayout y :*: U1)
    === func x y
  it "satisfies constraints" $
    checkCircuit
      (compileV2 id $ func @(Elem a))
      (\((x, y) :: (x a, y a)) -> toLayout x :*: toLayout y :*: U1)
