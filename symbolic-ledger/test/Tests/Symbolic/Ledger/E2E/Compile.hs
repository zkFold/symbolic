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
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))
import ZkFold.Symbolic.Interpreter (runInterpreter)
import Prelude (Semigroup ((<>)), Show (..), ($))
import Prelude qualified as Haskell
import ZkFold.Protocol.Plonkup.Input (PlonkupInput(..))
import ZkFold.Protocol.Plonkup.Verifier.Setup (PlonkupVerifierSetup(..))
import ZkFold.Symbolic.Data.Class (arithmetize, payload)

import Tests.Symbolic.Ledger.E2E.Two
import ZkFold.Symbolic.Ledger.Circuit.Compile (
  LedgerCircuitGates,
  LedgerContractInput (..),
  PlonkupTs,
  ledgerCircuit,
  ledgerSetup, ledgerProof, LedgerContractCompiledInput,
 )
import ZkFold.Protocol.Plonkup.Relation (PlonkupRelation(pubInput))

specE2ECompile :: Spec
specE2ECompile =
  it "E2E ledger circuit: prove and verify" $ do
    let lci :: LedgerContractInput Bi Bo Ud A Ixs Oxs TxCount I
        lci =
          LedgerContractInput
            { lciPreviousState = prevState
            , lciTransactionBatch = batch
            , lciNewState = newState
            , lciStateWitness = witness
            }

    ts :: TrustedSetup (LedgerCircuitGates + 6) <- powersOfTauSubset
    let compiledCircuit = ledgerCircuit @Bi @Bo @Ud @A @Ixs @Oxs @TxCount @I
    Haskell.print $ "constraints: " <> show (acSizeN compiledCircuit) <> ", variables: " <> show (acSizeM compiledCircuit)
    let proverSecret = PlonkupProverSecret (pure zero)

    let zkLedgerSetup =
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
    let zkLedgerProof = ledgerProof @ByteString ts proverSecret compiledCircuit lci
    let witnessInputs   = runInterpreter $ arithmetize lci
    let compiledInput   = (witnessInputs :*: U1) :*: (payload lci :*: U1)
    let PlonkupVerifierSetup { relation } = zkLedgerSetup
    let zkLedgerInput   = PlonkupInput (pubInput relation compiledInput)

    verify @(PlonkupTs (LedgerContractCompiledInput Bi Bo Ud A Ixs Oxs TxCount) LedgerCircuitGates ByteString)
      zkLedgerSetup zkLedgerInput zkLedgerProof `shouldBe` Haskell.True