{-# LANGUAGE AllowAmbiguousTypes #-}

-- | End-to-end tests for the IVC ledger step function.
--
-- We test two things:
--
-- 1. **Single step (interpreter)**
--    Run the *concrete* step function (via 'predicateEval') on a known valid
--    transaction and verify that the bridge-out count in the returned state is zero
--    (Example One's transaction does not produce bridge-out outputs).
--
-- 2. **Two-step chain (interpreter)**
--    Chain two steps and verify that the final UTxO leaf state matches the
--    expected state derived from Example One's 'newState2'.
module Tests.Symbolic.Ledger.E2E.IVC (specIVC) where

import GHC.Generics ((:.:) (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import ZkFold.Algebra.Class
import ZkFold.Data.Empty (empty)
import ZkFold.Data.Vector qualified as V
import ZkFold.Protocol.IVC.Predicate (Predicate, predicate, predicateEval)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (preimage)
import ZkFold.Symbolic.Interpreter (Interpreter (..))
import Prelude hiding ((+))

import ZkFold.Symbolic.Ledger.Circuit.IVC (
  LedgerIVCPayloadF,
  LedgerIVCStateF,
  TxStepPayload (..),
  TxStepState (..),
  ledgerTxStepFunction,
  mkInitialTxStepState,
  mkTxStepPayload,
 )
import ZkFold.Symbolic.Ledger.Examples.One qualified as Ex1
import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Types.Field (RollupBF, RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Validation.State (StateWitness (..))
import ZkFold.Symbolic.Ledger.Validation.Transaction (TransactionWitness)
import ZkFold.Symbolic.Ledger.Validation.TransactionBatch (TransactionBatchWitness (..))

-- ---------------------------------------------------------------------------
-- Type aliases (reuse Example One's small parameter values)
-- ---------------------------------------------------------------------------

type Ud = Ex1.Ud -- UTxO tree depth = 2

type Ixs = Ex1.Ixs -- tx inputs  = 1

type Oxs = Ex1.Oxs -- tx outputs = 1

type A = Ex1.A -- assets     = 1

type Bo = Ex1.Bo -- bridge-out = 1

-- ---------------------------------------------------------------------------
-- Extract the single TransactionWitness from each StateWitness
-- ---------------------------------------------------------------------------

txWitness1 :: TransactionWitness Ud Ixs Oxs A RollupBFInterpreter
txWitness1 =
  V.head (unComp1 (tbwTransactions (swTransactionBatch Ex1.witness)))

txWitness2 :: TransactionWitness Ud Ixs Oxs A RollupBFInterpreter
txWitness2 =
  V.head (unComp1 (tbwTransactions (swTransactionBatch Ex1.witness2)))

-- ---------------------------------------------------------------------------
-- Build TxStepPayloads from Example One data
-- ---------------------------------------------------------------------------

txPayload1 :: TxStepPayload Ud Ixs Oxs A Bo RollupBFInterpreter
txPayload1 =
  TxStepPayload
    { tspTx = Ex1.tx
    , tspWitness = txWitness1
    , tspBridgedOut = preimage (sBridgeOut Ex1.newState)
    }

txPayload2 :: TxStepPayload Ud Ixs Oxs A Bo RollupBFInterpreter
txPayload2 =
  TxStepPayload
    { tspTx = Ex1.tx2
    , tspWitness = txWitness2
    , tspBridgedOut = preimage (sBridgeOut Ex1.newState2)
    }

-- ---------------------------------------------------------------------------
-- Flat IVC state / payload for each step
-- ---------------------------------------------------------------------------

initialState :: LedgerIVCStateF Ud RollupBF
initialState = mkInitialTxStepState @Ud (sUTxO Ex1.newState)

ivcPayload1 :: LedgerIVCPayloadF Ud Ixs Oxs A Bo RollupBF
ivcPayload1 = mkTxStepPayload txPayload1

ivcPayload2 :: LedgerIVCPayloadF Ud Ixs Oxs A Bo RollupBF
ivcPayload2 = mkTxStepPayload txPayload2

-- ---------------------------------------------------------------------------
-- Compile the step function into a predicate (done once; ~100k-constraint circuit)
-- ---------------------------------------------------------------------------

stepPredicate
  :: Predicate
       RollupBF
       (LedgerIVCStateF Ud)
       (LedgerIVCPayloadF Ud Ixs Oxs A Bo)
stepPredicate = predicate (ledgerTxStepFunction @Ud @Ixs @Oxs @A @Bo)

-- Run one concrete step via the compiled circuit.
runStep
  :: LedgerIVCStateF Ud RollupBF
  -> LedgerIVCPayloadF Ud Ixs Oxs A Bo RollupBF
  -> LedgerIVCStateF Ud RollupBF
runStep = predicateEval stepPredicate

-- ---------------------------------------------------------------------------
-- Restore a flat LedgerIVCStateF back to a TxStepState for inspection
-- ---------------------------------------------------------------------------

-- | Convert the flat IVC state vector back into a 'TxStepState'.
--
-- 'LedgerIVCStateF ud = Layout (TxStepState ud) n'.
-- 'Payload (TxStepState ud) n' is structurally empty (all U1 positions);
-- we use 'empty' from "ZkFold.Data.Empty" to construct it.
restoreState
  :: LedgerIVCStateF Ud RollupBF
  -> TxStepState Ud RollupBFInterpreter
restoreState flatState = restore (Interpreter flatState, empty)

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

specIVC :: Spec
specIVC = describe "Ledger IVC step function" $ do
  it "bridge-out count is zero after step 1 (tx has no bridge-out outputs)" $ do
    let afterStep1 = runStep initialState ivcPayload1
    tssBOCount (restoreState afterStep1)
      `shouldBe` (zero :: FieldElement RollupBFInterpreter)

  it "two-step chain produces the correct UTxO leaf state" $ do
    let afterStep1 = runStep initialState ivcPayload1
        afterStep2 = runStep afterStep1 ivcPayload2
        expected = restoreState (mkInitialTxStepState @Ud (sUTxO Ex1.newState2))
    tssLeaves (restoreState afterStep2)
      `shouldBe` tssLeaves expected
