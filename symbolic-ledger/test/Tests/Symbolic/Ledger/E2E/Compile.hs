module Tests.Symbolic.Ledger.E2E.Compile (specE2ECompile) where

import Test.Hspec (Spec)

-- import Tests.Symbolic.Ledger.E2E.Compile.IVC
import Tests.Symbolic.Ledger.E2E.Compile.One
import Tests.Symbolic.Ledger.E2E.Compile.Three
import Tests.Symbolic.Ledger.E2E.Compile.Two

specE2ECompile :: Spec
specE2ECompile = do
  specE2ECompileOne
  specE2ECompileTwo
  specE2ECompileThree
  -- specE2ECompileIVC  -- NOTE: compiles the recursive IVC circuit; very memory-intensive, run manually
