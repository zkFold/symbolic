{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}

module Tests.Symbolic.Data.Common (
  specConstantRoundtrip,
  specSymbolicFunction0,
  specSymbolicFunction1,
  specSymbolicFunction1WithPar,
  specSymbolicFunction2,
) where

import Control.Applicative (pure)
import Data.Binary (Binary)
import Data.Eq (Eq)
import Data.Function (id, ($))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Type.Equality (type (~))
import GHC.Generics (U1 (..), type (:*:) (..))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Arbitrary, Gen, (===))
import Text.Show (Show)

import Tests.Common (it)
import ZkFold.Algebra.Class
import ZkFold.ArithmeticCircuit (checkCircuit, checkClosedCircuit, eval, exec)
import ZkFold.ArithmeticCircuit.Elem (Elem, compile)
import ZkFold.ArithmeticCircuit.Node (Input, Output, SymbolicFunction)
import ZkFold.Symbolic.Class (Arithmetic, Symbolic)
import ZkFold.Symbolic.Data.Class (RepData, SymbolicData (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput)

specConstantRoundtrip
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
specConstantRoundtrip symType hType gen = do
  it (symType <> " embeds " <> hType) \(x :: x a) ->
    fromConstant (toConstant x) === x
  it (hType <> " embeds " <> symType) do
    (x :: Const (x a)) <- gen
    pure $ toConstant (fromConstant x :: x a) === x

type Match a x = (Show (x a), Binary a, Show a)

type MatchingSymbolicInput a x =
  ( SymbolicInput x
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

specSymbolicFunction0
  :: forall a x
   . (Arithmetic a, MatchingSymbolicInput a x, MatchingSymbolicOutput a x)
  => String -> (forall c. Symbolic c => x c) -> Spec
specSymbolicFunction0 desc v = describe desc do
  it "evaluates correctly" $
    fromLayout (exec $ compile id $ v @(Elem a)) === v
  it "satisfies constraints" $
    checkClosedCircuit (compile id $ v @(Elem a))

specSymbolicFunction1
  :: forall a x y
   . (Arithmetic a, MatchingSymbolicInput a x, MatchingSymbolicOutput a y)
  => String -> (forall c. Symbolic c => x c -> y c) -> Spec
specSymbolicFunction1 desc func = describe desc do
  it "evaluates correctly" \(x :: x a) ->
    fromLayout (eval (compile (:*: U1) $ func @(Elem a)) $ toLayout x)
      === func x
  it "satisfies constraints" $
    checkCircuit
      (compile (:*: U1) $ func @(Elem a))
      (\(x :: x a) -> toLayout x)

specSymbolicFunction1WithPar
  :: forall p a x y
   . ( Arbitrary p
     , Show p
     , Arithmetic a
     , MatchingSymbolicInput a x
     , MatchingSymbolicOutput a y
     )
  => String -> (forall c. Symbolic c => p -> x c -> y c) -> Spec
specSymbolicFunction1WithPar desc func = describe desc do
  it "evaluates correctly" \p (x :: x a) ->
    fromLayout (eval (compile (:*: U1) $ func @(Elem a) p) $ toLayout x)
      === func p x
  it "satisfies constraints" \p ->
    checkCircuit
      (compile (:*: U1) $ func @(Elem a) p)
      (\(x :: x a) -> toLayout x)

specSymbolicFunction2
  :: forall a x y z
   . ( Arithmetic a
     , MatchingSymbolicInput a x
     , MatchingSymbolicInput a y
     , MatchingSymbolicOutput a z
     )
  => String -> (forall c. Symbolic c => x c -> y c -> z c) -> Spec
specSymbolicFunction2 desc func = describe desc do
  it "evaluates correctly" \(x :: x a) (y :: y a) ->
    fromLayout
      (eval (compile id $ func @(Elem a)) $ toLayout x :*: toLayout y :*: U1)
      === func x y
  it "satisfies constraints" $
    checkCircuit
      (compile id $ func @(Elem a))
      (\((x, y) :: (x a, y a)) -> toLayout x :*: toLayout y :*: U1)
