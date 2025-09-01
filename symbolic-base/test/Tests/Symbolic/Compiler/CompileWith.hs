{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Symbolic.Compiler.CompileWith (specCompileWith) where

import Data.Binary (Binary)
import Data.Function (($))
import Data.Functor.Rep (Rep, Representable)
import Data.Map (Map, fromList)
import GHC.Generics (U1 (..), (:*:) (..))
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), (===))
import Text.Show (Show)

import ZkFold.ArithmeticCircuit (
  ArithmeticCircuit,
  acContext,
  guessOutput,
  witnessGenerator,
 )
import ZkFold.ArithmeticCircuit.Context (getAllVars)
import ZkFold.ArithmeticCircuit.Var (NewVar)
import ZkFold.Data.Product (toPair)
import ZkFold.Symbolic.Class (Arithmetic, Symbolic)
import ZkFold.Symbolic.Compiler (compileWith)
import ZkFold.Symbolic.Data.Bool ((&&))
import ZkFold.Symbolic.Data.ByteString (ByteString)
import ZkFold.Symbolic.Data.Vec (runVec)

testFunction :: Symbolic c => ByteString 256 c -> ByteString 256 c -> ByteString 256 c
testFunction = (&&)

witGen
  :: (Arithmetic a, Representable i, Binary (Rep i))
  => ArithmeticCircuit a i o
  -> i a
  -> Map NewVar a
witGen circuit input =
  let wg = witnessGenerator circuit input
   in fromList [(v, wg v) | v <- getAllVars (acContext circuit)]

specCompileWith :: forall a. (Arbitrary a, Arithmetic a, Binary a, Show a) => Spec
specCompileWith = describe "CompileWith specification" $ do
  prop "Guessing with payload is constant in input" $
    let circuit =
          runVec $ compileWith @a
            (guessOutput toPair)
            (\(p :*: q) -> (U1 :*: U1 :*: U1, p :*: q :*: U1))
            testFunction
     in \x p q -> witGen circuit (x :*: p) === witGen circuit (x :*: q)
