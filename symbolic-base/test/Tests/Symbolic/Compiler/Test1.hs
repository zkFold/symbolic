{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.Symbolic.Compiler.Test1 (specArithmetization1) where

import           Data.Binary                       (Binary)
import           GHC.Generics                      (Par1 (..), U1 (..), (:*:) (..))
import           Numeric.Natural                   (Natural)
import           Prelude                           hiding (Bool, Eq (..), Num (..), not, replicate, (/), (>), (^), (||))
import qualified Prelude                           as Haskell
import           Test.Hspec
import           Test.QuickCheck

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Symbolic.Class             (Arithmetic, Symbolic)
import           ZkFold.Symbolic.Compiler
import           ZkFold.Symbolic.Data.Bool         (Bool (..))
import           ZkFold.Symbolic.Data.Conditional  ((?))
import           ZkFold.Symbolic.Data.Eq           (Eq (..))
import           ZkFold.Symbolic.Data.FieldElement (FieldElement)
import           ZkFold.Symbolic.Interpreter       (Interpreter)

-- f x y = if (2 / x > y) then (x ^ 2 + 3 * x + 5) else (4 * x ^ 3)
testFunc :: forall c . Symbolic c => FieldElement c -> FieldElement c -> FieldElement c
testFunc x y =
    let c  = fromConstant @Integer @(FieldElement c)
        g1 = x ^ (2 :: Natural) + c 3 * x + c 5
        g2 = c 4 * x ^ (3 :: Natural)
        g3 = c 2 // x
    in (g3 == y :: Bool c) ? g1 $ g2

testResult ::
  forall a . (Arithmetic a, Binary a) =>
  ArithmeticCircuit a ((U1 :*: U1 :*: U1) :*: Par1 :*: Par1 :*: U1) Par1 ->
  a -> a -> Haskell.Bool
testResult r x y = fromConstant (unPar1 $ eval r ((U1 :*: U1 :*: U1) :*: Par1 x :*: Par1 y :*: U1)) Haskell.==
    testFunc @(Interpreter a) (fromConstant x) (fromConstant y)

specArithmetization1 :: forall a . (Arithmetic a, Arbitrary a, Binary a, Show a) => Spec
specArithmetization1 = do
    describe "Arithmetization test 1" $ do
        it "should pass" $ do
            let ac = compile @a testFunc
            property $ \x y -> testResult ac x y
