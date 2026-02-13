module Tests.Symbolic.Ledger.E2E.Compile.Three (specE2ECompileThree) where

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

import ZkFold.Symbolic.Ledger.Circuit.Compile (
  LedgerCircuitGates,
  LedgerContractCompiledInput,
  LedgerContractInput (..),
  PlonkupTs,
  ledgerCircuit,
  ledgerProof,
  ledgerSetup,
 )
import ZkFold.Symbolic.Ledger.Examples.Three

specE2ECompileThree :: Spec
specE2ECompileThree =
  it "E2E ledger circuit, Three: prove and verify" $ do
    ts :: TrustedSetup (LedgerCircuitGates + 6) <- powersOfTauSubset
    let lci :: LedgerContractInput Bi Bo Ud A Ixs Oxs TxCount I
        lci =
          LedgerContractInput
            { lciPreviousState = prevState
            , lciTransactionBatch = batch
            , lciNewState = newState
            , lciStateWitness = witness
            }
        lci2 =
          LedgerContractInput
            { lciPreviousState = newState
            , lciTransactionBatch = batch2
            , lciNewState = newState2
            , lciStateWitness = witness2
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
      zkLedgerProof2 = ledgerProof @ByteString ts proverSecret compiledCircuit lci2
      witnessInputs = runInterpreter $ arithmetize lci
      witnessInputs2 = runInterpreter $ arithmetize lci2
      compiledInput = (witnessInputs :*: U1) :*: (payload lci :*: U1)
      compiledInput2 = (witnessInputs2 :*: U1) :*: (payload lci2 :*: U1)
      PlonkupVerifierSetup {relation} = zkLedgerSetup
      zkLedgerInput = PlonkupInput (pubInput relation compiledInput)
      zkLedgerInput2 = PlonkupInput (pubInput relation compiledInput2)
    Haskell.putStrLn $ "zkLedgerInput: " <> show zkLedgerInput
    verify @(PlonkupTs Bi Bo A (LedgerContractCompiledInput Bi Bo Ud A Ixs Oxs TxCount) LedgerCircuitGates ByteString)
      zkLedgerSetup
      zkLedgerInput
      zkLedgerProof
      `shouldBe` Haskell.True
    verify @(PlonkupTs Bi Bo A (LedgerContractCompiledInput Bi Bo Ud A Ixs Oxs TxCount) LedgerCircuitGates ByteString)
      zkLedgerSetup
      zkLedgerInput2
      zkLedgerProof2
      `shouldBe` Haskell.True
