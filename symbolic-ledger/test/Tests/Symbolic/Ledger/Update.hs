module Tests.Symbolic.Ledger.Update (specUpdateLedgerState) where

import Test.Hspec (Spec, describe)
import Prelude (($))

import Tests.Symbolic.Ledger.E2E.Compile (specE2ECompile)
import Tests.Symbolic.Ledger.E2E.One (specE2EOne)
import Tests.Symbolic.Ledger.E2E.Two (specE2ETwo)
import Tests.Symbolic.Ledger.E2E.Three (specE2EThree)

specUpdateLedgerState :: Spec
specUpdateLedgerState = describe "updateLedgerState" $ do
  specE2EOne
  specE2ETwo
  specE2EThree
  specE2ECompile
