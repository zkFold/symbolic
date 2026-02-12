module Tests.Symbolic.Ledger.E2E.Three (
  specE2EThree,
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

import Test.Hspec (Spec, it, shouldBe)
import ZkFold.Algebra.Class
import ZkFold.Symbolic.Data.Bool (true)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import Prelude (($))
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Examples.Three
import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.State (validateStateUpdateIndividualChecks)

-- End-to-end test for a slightly intricate case.
specE2EThree :: Spec
specE2EThree =
  it "E2E Three" $ do
    let
    sLength newState `shouldBe` (one :: FieldElement I)
    validateStateUpdateIndividualChecks prevState batch newState witness `shouldBe` Haskell.pure true
