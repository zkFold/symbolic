{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Arithmetization.Test3 (specArithmetization3) where

import Test.Hspec
import ZkFold.Base.Algebra.Basic.Field (Zp)
import ZkFold.Base.Algebra.Basic.Number (Prime)
import ZkFold.Symbolic.Compiler
import ZkFold.Symbolic.Data.Bool (Bool (..))
import ZkFold.Symbolic.Data.Ord (Ord (..))
import ZkFold.Symbolic.Types (Symbolic)
import Prelude hiding (Bool, Eq (..), Num (..), Ord (..), any, not, replicate, (/), (^), (||))

instance Prime 97

type R = ArithmeticCircuit (Zp 97)

-- A comparison test
testFunc :: forall a. (Symbolic a) => a -> a -> Bool a
testFunc x y = x <= y

specArithmetization3 :: Spec
specArithmetization3 = do
  describe "Arithmetization test 3" $ do
    it "should pass" $ do
      let Bool r = compile @(Zp 97) (testFunc @R) :: Bool R
      Bool (acValue (applyArgs r [3, 5])) `shouldBe` testFunc 3 5
