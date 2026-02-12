module Tests.Symbolic.Ledger.E2E.Compile (specE2ECompile) where

import Test.Hspec (Spec)
import Tests.Symbolic.Ledger.E2E.Compile.One
import Tests.Symbolic.Ledger.E2E.Compile.Two
import Tests.Symbolic.Ledger.E2E.Compile.Three

specE2ECompile :: Spec
specE2ECompile = do
  -- specE2ECompileOne
  -- specE2ECompileTwo
  specE2ECompileThree
