module Tests.Symbolic.Ledger.E2E.Four (
  specE2EFour,
) where

import Test.Hspec (Spec, it, shouldBe)
import ZkFold.Symbolic.Data.Bool (true)
import Prelude (($))
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Examples.Four
import ZkFold.Symbolic.Ledger.Validation.State (validateStateUpdateIndividualChecks)

-- | Test that a batch containing a null transaction (all null inputs, all null outputs)
-- passes validation. Null transactions are safe as batch padding because they are
-- pure no-ops on the UTxO state: they produce no outputs (so transaction ID uniqueness
-- is irrelevant) and trivially satisfy the balance constraint.
specE2EFour :: Spec
specE2EFour =
  it "Null transaction as batch padding passes validation" $ do
    let (checks1, _, _, _) = validateStateUpdateIndividualChecks prevState batch newState witness
    checks1 `shouldBe` Haskell.pure true
