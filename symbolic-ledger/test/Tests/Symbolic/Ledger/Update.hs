module Tests.Symbolic.Ledger.Update (specUpdateLedgerState) where

import Test.Hspec (Spec, describe)
import Prelude (($))

import Tests.Symbolic.Ledger.E2E.Compile (specE2ECompile)
import Tests.Symbolic.Ledger.E2E.One (specE2EOne)
import Tests.Symbolic.Ledger.E2E.Two (specE2ETwo)

specUpdateLedgerState :: Spec
specUpdateLedgerState = describe "updateLedgerState" $ do
  specE2EOne
  specE2ETwo
  -- specE2ECompile -- TODO: Enable it back, see https://github.com/zkFold/symbolic/issues/769.
