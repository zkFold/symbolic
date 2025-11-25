module Tests.Symbolic.Ledger.E2E.Compile (specE2ECompile) where

import Control.Applicative (pure)
import Data.ByteString (ByteString)
import GHC.Generics (U1 (..), (:*:) (..))
import GHC.TypeNats (type (+))
import Test.Hspec (Spec, it, shouldBe)
import ZkFold.Algebra.Class
import ZkFold.ArithmeticCircuit (acSizeM, acSizeN)
import ZkFold.Protocol.NonInteractiveProof (
  NonInteractiveProof (verify),
  TrustedSetup (..),
  powersOfTauSubset,
 )
import ZkFold.Protocol.Plonkup.Input (PlonkupInput (..))
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))
import ZkFold.Protocol.Plonkup.Relation (PlonkupRelation (pubInput))
import ZkFold.Protocol.Plonkup.Verifier.Setup (PlonkupVerifierSetup (..))
import ZkFold.Symbolic.Data.Class (arithmetize, payload)
import ZkFold.Symbolic.Interpreter (runInterpreter)
import Prelude (Semigroup ((<>)), Show (..), ($))
import Prelude qualified as Haskell

import Tests.Symbolic.Ledger.E2E.One
import ZkFold.Symbolic.Ledger.Circuit.Compile (
  LedgerCircuitGates,
  LedgerContractCompiledInput,
  LedgerContractInput (..),
  PlonkupTs,
  ledgerCircuit,
  ledgerProof,
  ledgerSetup,
 )

specE2ECompile :: Spec
specE2ECompile =
  it "E2E ledger circuit: prove and verify" $ do
    ts :: TrustedSetup (LedgerCircuitGates + 6) <- powersOfTauSubset
    let lci :: LedgerContractInput Bi Bo Ud A Ixs Oxs TxCount I
        lci =
          LedgerContractInput
            { lciPreviousState = prevState
            , lciTransactionBatch = batch
            , lciNewState = newState
            , lciStateWitness = witness
            }
    let compiledCircuit = ledgerCircuit @Bi @Bo @Ud @A @Ixs @Oxs @TxCount @I
    Haskell.putStrLn $
      "constraints: " <> show (acSizeN compiledCircuit) <> ", variables: " <> show (acSizeM compiledCircuit)
    let
      proverSecret = PlonkupProverSecret (pure zero)
      zkLedgerSetup =
        ledgerSetup
          @ByteString
          @Bi
          @Bo
          @Ud
          @A
          @Ixs
          @Oxs
          @TxCount
          @I
          ts
          compiledCircuit
      zkLedgerProof = ledgerProof @ByteString ts proverSecret compiledCircuit lci
      witnessInputs = runInterpreter $ arithmetize lci
      compiledInput = (witnessInputs :*: U1) :*: (payload lci :*: U1)
      PlonkupVerifierSetup {relation} = zkLedgerSetup
      zkLedgerInput = PlonkupInput (pubInput relation compiledInput)
    verify @(PlonkupTs (LedgerContractCompiledInput Bi Bo Ud A Ixs Oxs TxCount) LedgerCircuitGates ByteString)
      zkLedgerSetup
      zkLedgerInput
      zkLedgerProof
      `shouldBe` Haskell.True
