{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-partial-type-signatures #-}

-- | End-to-end IVC compile test.
--
-- This is the IVC analog of @Tests.Symbolic.Ledger.E2E.Compile.Three@:
--
-- 1. Compile the IVC step predicate circuit and report its size.
-- 2. Run @ledgerIVCSetup@ (first step + empty accumulator).
-- 3. Run @ledgerIVCProve@ (second step + accumulation fold).
-- 4. Verify the result via the accumulator @decider@.
--
-- NOTE: Steps 2–4 compile the *recursive* step circuit (step function +
-- embedded recursive verifier), which is significantly larger than the
-- monolithic ledger circuit.  Expect long runtimes.
module Tests.Symbolic.Ledger.E2E.Compile.IVC (specE2ECompileIVC) where

import Control.Exception (evaluate)
import Data.Foldable (all, toList)
import GHC.Generics ((:.:) (..))
import Prelude (Semigroup ((<>)), Show (..), ($), (&&), (.), (==))
import Prelude qualified as Haskell

import Test.Hspec (Spec, it, shouldBe)
import ZkFold.Algebra.Class
import ZkFold.ArithmeticCircuit (acSizeM, acSizeN)
import ZkFold.ArithmeticCircuit.Context (CircuitContext)
import ZkFold.Data.Vector qualified as V
import ZkFold.Protocol.IVC.AccumulatorScheme (AccumulatorScheme (..), accumulatorScheme)
import ZkFold.Protocol.IVC.Commit (cyclicCommit)
import ZkFold.Protocol.IVC.Internal (IVCResult (..))
import ZkFold.Protocol.IVC.OperationRecord (OperationRecord (..), opValue)
import ZkFold.Protocol.IVC.Oracle (OracleSource (..), mimcHash)
import ZkFold.Protocol.IVC.Predicate (Predicate (..), predicate)
import ZkFold.Protocol.IVC.RecursiveFunction (recursiveFunction, recursivePredicate)
import ZkFold.Protocol.IVC.WeierstrassWitness (WeierstrassWitness)
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.Hash (preimage)
import ZkFold.Symbolic.Interpreter (Interpreter (..))

import Tests.Symbolic.Ledger.E2E.Utils (time)
import ZkFold.Symbolic.Ledger.Circuit.IVC (
  LedgerIVCPayloadF,
  LedgerIVCStateF,
  TxStepPayload (..),
  ledgerIVCProve,
  ledgerIVCSetup,
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
-- Type aliases
-- ---------------------------------------------------------------------------

type Ud = Ex1.Ud

type Ixs = Ex1.Ixs

type Oxs = Ex1.Oxs

type A = Ex1.A

type Bo = Ex1.Bo

-- | Protostar accumulator degree (= circuit degree for polynomial constraints).
type D = 2

-- | Number of NARK proofs in the accumulator (= 1 for the Protostar IVC scheme).
type K = 1

-- | Recursive point type: BLS12-381 G1 point stored as a Weierstrass witness.
type PT = WeierstrassWitness

-- | Commitment type: an 'OperationRecord' of EC point operations.
type C = OperationRecord RollupBF (WeierstrassWitness (Interpreter RollupBF))

-- ---------------------------------------------------------------------------
-- Orphan instances required by 'accumulatorScheme'
-- ---------------------------------------------------------------------------

-- | Extract oracle randomness from a commitment @C@.
-- Delegates to the @WeierstrassWitness@ instance, whose @payload@ is @U1@,
-- so @source@ always returns @[]@. This makes the oracle deterministic,
-- which is acceptable for a testing context.
instance OracleSource RollupBF C where
  source = source . opValue

-- | Scale a @WeierstrassWitness (Interpreter RollupBF)@ by a concrete
-- base-field scalar @RollupBF@.
-- Coerces @k :: RollupBF@ to @FieldElement (Interpreter RollupBF)@ first
-- because the existing @Scale@ instance for @WeierstrassWitness@ expects
-- the scalar in the @FieldElement@ wrapper.
instance {-# OVERLAPPING #-} Scale RollupBF (WeierstrassWitness (Interpreter RollupBF)) where
  scale k = scale @(FieldElement (Interpreter RollupBF)) (fromConstant k)

-- | Required by 'IsRecursivePoint WeierstrassWitness RollupBF':
-- 'IsRecursivePoint pt a' demands
-- 'OracleSource (FieldElement (CircuitContext a)) (pt (CircuitContext a))'.
-- The circuit-context point carries no concrete randomness, so we return @[]@.
instance {-# OVERLAPPING #-} OracleSource (FieldElement (CircuitContext RollupBF)) (WeierstrassWitness (CircuitContext RollupBF)) where
  source _ = []

-- ---------------------------------------------------------------------------
-- IVC inputs derived from Example One
-- ---------------------------------------------------------------------------

txWitness1 :: TransactionWitness Ud Ixs Oxs A RollupBFInterpreter
txWitness1 =
  V.head (unComp1 (tbwTransactions (swTransactionBatch Ex1.witness)))

txWitness2 :: TransactionWitness Ud Ixs Oxs A RollupBFInterpreter
txWitness2 =
  V.head (unComp1 (tbwTransactions (swTransactionBatch Ex1.witness2)))

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

-- | IVC initial state: UTxO tree of 'Ex1.newState' (post bridge-in).
initialState :: LedgerIVCStateF Ud RollupBF
initialState = mkInitialTxStepState @Ud (sUTxO Ex1.newState)

-- | IVC payload for the first step (processes 'Ex1.tx').
ivcPayload1 :: LedgerIVCPayloadF Ud Ixs Oxs A Bo RollupBF
ivcPayload1 = mkTxStepPayload txPayload1

-- | IVC payload for the second step (processes 'Ex1.tx2').
ivcPayload2 :: LedgerIVCPayloadF Ud Ixs Oxs A Bo RollupBF
ivcPayload2 = mkTxStepPayload txPayload2

-- ---------------------------------------------------------------------------
-- Test spec
-- ---------------------------------------------------------------------------

specE2ECompileIVC :: Spec
specE2ECompileIVC =
  it "E2E IVC ledger step circuit: two-step prove and decider check" $ do
    -- Report IVC step circuit size (the base circuit, not the recursive one).
    let stepPred = predicate (ledgerTxStepFunction @Ud @Ixs @Oxs @A @Bo)
    Haskell.putStrLn $
      "IVC step circuit constraints: "
        <> show (acSizeN (predicateCircuit stepPred))
        <> ", variables: "
        <> show (acSizeM (predicateCircuit stepPred))

    -- Run IVC setup: process the first transaction and initialise the
    -- (empty) accumulator.  Internally compiles the *recursive* step
    -- circuit (base step + verifier), which is expensive.
    ivcResult1 <-
      time "ledgerIVCSetup" $
        evaluate $
          ledgerIVCSetup @D @PT @Ud @Ixs @Oxs @A @Bo
            mimcHash
            (cyclicCommit @C)
            (cyclicCommit @(WeierstrassWitness (CircuitContext RollupBF)))
            initialState
            ivcPayload1

    -- Run IVC prove: fold the second transaction into the accumulator.
    ivcResult2 <-
      time "ledgerIVCProve" $
        evaluate $
          ledgerIVCProve @D @PT @Ud @Ixs @Oxs @A @Bo
            mimcHash
            (cyclicCommit @C)
            (cyclicCommit @(WeierstrassWitness (CircuitContext RollupBF)))
            opValue
            ivcResult1
            ivcPayload2

    -- Verify via the accumulator decider.
    -- decider returns (Vector 1 C, C); both components are zero on success.
    let pRec =
          recursivePredicate @PT @D @K $
            recursiveFunction @PT @D @K
              mimcHash
              (cyclicCommit @(WeierstrassWitness (CircuitContext RollupBF)))
              (ledgerTxStepFunction @Ud @Ixs @Oxs @A @Bo)
        -- Annotate pt = WeierstrassWitness (Interpreter RollupBF) explicitly
        -- via a partial type signature, since pt appears only in constraints
        -- of accumulatorScheme (not in any argument type) and would otherwise
        -- be ambiguous.  The wildcard _ lets GHC infer the RecursiveI functor.
        accScheme :: AccumulatorScheme D K _ C (WeierstrassWitness (Interpreter RollupBF)) RollupBF
        accScheme = accumulatorScheme @D mimcHash (cyclicCommit @C) pRec
        (cv, c) =
          decider accScheme (_acc ivcResult2)
            :: (V.Vector K C, C)
    (all (== zero) (toList cv) && c == zero) `shouldBe` Haskell.True
