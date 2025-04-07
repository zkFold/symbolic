{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.Symbolic.Compiler.Test4 (specArithmetization4) where

import           GHC.Generics                                (Par1 (..), U1 (..), (:*:) (..))
import           Prelude                                     hiding (Bool, Eq (..), Num (..), Ord (..), (&&))
import qualified Prelude                                     as Haskell
import           Test.Hspec                                  (Spec, describe, it)
import           Test.QuickCheck                             (Testable (..), (==>))

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..), one, zero)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import           ZkFold.Base.Algebra.EllipticCurve.Class     (CyclicGroup (..))
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile, eval)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Data.FieldElement           (FieldElement)

type C = BLS12_381_G1_Point
type F = ScalarFieldOf C

lockedByTxId :: forall a c . (FromConstant a (FieldElement c), Symbolic c) => a -> FieldElement c -> Bool c
lockedByTxId targetValue inputValue = inputValue == fromConstant targetValue

testSameValue :: F -> Haskell.Bool
testSameValue targetValue =
    let Bool ac = compile @F (lockedByTxId @F targetValue) :: Bool (ArithmeticCircuit F ((U1 :*: U1) :*: Par1 :*: U1))
        b       = unPar1 (eval ac ((U1 :*: U1) :*: Par1 targetValue :*: U1))
    in b Haskell.== one

testDifferentValue :: F -> F -> Haskell.Bool
testDifferentValue targetValue otherValue =
    let Bool ac = compile @F (lockedByTxId @F targetValue) :: Bool (ArithmeticCircuit F ((U1 :*: U1) :*: Par1 :*: U1))
        b       = unPar1 (eval ac ((U1 :*: U1) :*: Par1 otherValue :*: U1))
    in b Haskell.== zero

specArithmetization4 :: Spec
specArithmetization4 = do
    describe "LockedByTxId arithmetization test 1" $ do
        it "should pass" $ property testSameValue
    describe "LockedByTxId arithmetization test 2" $ do
        it "should pass" $ property $ \x y -> x Haskell./= y ==> testDifferentValue x y
