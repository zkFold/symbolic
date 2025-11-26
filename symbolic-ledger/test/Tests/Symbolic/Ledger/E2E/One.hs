{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Tests.Symbolic.Ledger.E2E.One (
  specE2EOne,
  prevState,
  batch,
  witness,
  newState,
  I,
  Bi,
  Bo,
  Ud,
  A,
  Ixs,
  Oxs,
  TxCount,
) where

import Control.Applicative (pure)
import GHC.Generics ((:.:) (..))
import Test.Hspec (Spec, it, shouldBe)
import ZkFold.Algebra.Class
import ZkFold.Symbolic.Data.Bool (true)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import Prelude (($))
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.State (validateStateUpdateIndividualChecks)
import ZkFold.Symbolic.Ledger.Examples.One

-- End-to-end test for a very simplified case.
specE2EOne :: Spec
specE2EOne =
  it "E2E One" $ do
    sLength newState `shouldBe` (one :: FieldElement I)
    validateStateUpdateIndividualChecks prevState batch newState witness `shouldBe` Haskell.pure true
    validateStateUpdateIndividualChecks newState batch2 newState2 witness2 `shouldBe` Haskell.pure true
    unComp1 utxoPreimage3 `shouldBe` pure (nullUTxO @A @I)
