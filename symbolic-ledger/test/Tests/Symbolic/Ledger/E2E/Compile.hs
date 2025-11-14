module Tests.Symbolic.Ledger.E2E.Compile (specE2ECompile) where

import Control.Applicative (pure)
import Data.ByteString (ByteString)
import GHC.Generics (U1 (..), (:*:) (..))
import GHC.Natural (Natural)
import GHC.TypeNats (type (+))
import Test.Hspec (Spec, it, shouldBe)
import ZkFold.Algebra.Class
import ZkFold.Algebra.Number qualified as Number
import ZkFold.ArithmeticCircuit (acSizeM, acSizeN)
import ZkFold.Protocol.NonInteractiveProof (
  NonInteractiveProof (prove, setupProve, setupVerify, verify),
  TrustedSetup (..),
  powersOfTauSubset,
 )
import ZkFold.Protocol.Plonkup (Plonkup (..))
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))
import ZkFold.Protocol.Plonkup.Utils (getParams)
import ZkFold.Protocol.Plonkup.Witness (PlonkupWitnessInput (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Interpreter (runInterpreter)
import Prelude (Semigroup ((<>)), Show (..), ($))
import Prelude qualified as Haskell

import Tests.Symbolic.Ledger.E2E.Two
import ZkFold.Symbolic.Ledger.Circuit.Compile (
  LedgerCircuit,
  LedgerCircuitGates,
  LedgerContractInput (..),
  LedgerContractOutput,
  PlonkupTs,
  ZKProofBytes,
  ZKSetupBytes,
  ledgerCircuit,
  ledgerSetup,
  mkProof,
  mkSetup,
 )
import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Types.Field (RollupBF, RollupBFInterpreter)

specE2ECompile :: Spec
specE2ECompile =
  it "E2E ledger circuit: prove and verify" $ do
    -- let lci :: LedgerContractInput Bi Bo Ud A Ixs Oxs TxCount I
    --     lci =
    --       LedgerContractInput
    --         { lciPreviousState = prevState
    --         , lciTransactionBatch = batch
    --         , lciNewState = newState
    --         , lciStateWitness = witness
    --         }

    -- ts :: TrustedSetup (LedgerCircuitGates + 6) <- powersOfTauSubset
    let compiledCircuit = ledgerCircuit @Bi @Bo @Ud @A @Ixs @Oxs @TxCount @I
    Haskell.print $ "constraints: " <> show (acSizeN compiledCircuit) <> ", variables: " <> show (acSizeM compiledCircuit)

-- let setupV =
--       ledgerSetup
--         @ByteString
--         @Bi
--         @Bo
--         @Ud
--         @A
--         @Ixs
--         @Oxs
--         @TxCount
--         @I
--         ts

-- let zkSetupBytes :: ZKSetupBytes
--     zkSetupBytes = mkSetup setupV

-- verify setupV inputV proof `shouldBe` Haskell.True
