-- TODO: Is this language extension needed?
{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Symbolic.Ledger.Circuit.Compile (
  ledgerCircuit,
) where

-- TODO: Refine import from SmartWallet.

import Data.Type.Equality (type (~))
import GHC.Generics (Generic, Generic1, Par1, U1, (:*:))
import GHC.TypeNats (type (+), type (^))
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_JacobianPoint, BLS12_381_G2_JacobianPoint)
import ZkFold.Algebra.EllipticCurve.Jubjub (Fq)
import ZkFold.Algebra.Number qualified as Number
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.ArithmeticCircuit
import ZkFold.Protocol.NonInteractiveProof as NP (
  FromTranscript (..),
  NonInteractiveProof (..),
  ToTranscript (..),
  TrustedSetup (..),
  powersOfTauSubset,
 )
import ZkFold.Protocol.Plonkup (Plonkup (..))
import ZkFold.Protocol.Plonkup.Proof
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))
import ZkFold.Protocol.Plonkup.Relation (PlonkupRelation (..))
import ZkFold.Protocol.Plonkup.Utils (getParams)
import ZkFold.Protocol.Plonkup.Verifier.Commitments
import ZkFold.Protocol.Plonkup.Verifier.Setup
import ZkFold.Protocol.Plonkup.Witness (PlonkupWitnessInput (..))
import ZkFold.Symbolic.Class (BaseField)
import ZkFold.Symbolic.Compiler qualified as C
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Vec (Vec (..), runVec)
import ZkFold.Symbolic.Examples.SmartWallet hiding (PlonkupTs)
import Prelude (($))

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.State

data LedgerContractInput bi bo ud a i o t c = LedgerContractInput
  { lciPreviousState :: State bi bo ud a c
  , lciTransactionBatch :: TransactionBatch i o a t c
  , lciNewState :: State bi bo ud a c
  , lciStateWitness :: StateWitness bi bo ud a i o t c
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

type LedgerContractOutput c = Bool c

ledgerContract
  :: forall bi bo ud a i o t c
   . SignatureState bi bo ud a c
  => SignatureTransactionBatch ud i o a t c
  => LedgerContractInput bi bo ud a i o t c -> LedgerContractOutput c
ledgerContract LedgerContractInput {..} = validateStateUpdate lciPreviousState lciTransactionBatch lciNewState lciStateWitness

-- TODO: Is this circuit gate count enough?
type LedgerCircuitGates = 2 ^ 18

type LedgerContractInputLayout bi bo ud a i o t =
  Layout
    (LedgerContractInput bi bo ud a i o t :*: U1)
    (Order Fq)

type LedgerContractInputPayload bi bo ud a i o t =
  Payload
    (LedgerContractInput bi bo ud a i o t :*: U1)
    (Order Fq)

type LedgerContractCompiledInput bi bo ud a i o t =
  LedgerContractInputPayload bi bo ud a i o t :*: LedgerContractInputLayout bi bo ud a i o t

type LedgerCircuit bi bo ud a i o t = ArithmeticCircuit Fq (LedgerContractCompiledInput bi bo ud a i o t) Par1

ledgerCircuit
  :: forall bi bo ud a i o t c
   . SignatureState bi bo ud a c
  => SignatureTransactionBatch ud i o a t c
  => -- Since we are hardcoding @Fq@ at some places in this file, it is important that it is the same as the base field of the context.
  Fq ~ BaseField c
  => LedgerCircuit bi bo ud a i o t
ledgerCircuit = runVec $ C.compile @Fq ledgerContract

type PlonkupTs i n t = Plonkup i Par1 n BLS12_381_G1_JacobianPoint BLS12_381_G2_JacobianPoint t (PolyVec Fq)

ledgerSetup
  :: forall tc bi bo ud a i o t c
   . TranscriptConstraints tc
  => SignatureState bi bo ud a c
  => SignatureTransactionBatch ud i o a t c
  => TrustedSetup (LedgerCircuitGates + 6)
  -> LedgerCircuit bi bo ud a i o t
  -> SetupVerify (PlonkupTs (LedgerContractCompiledInput bi bo ud a i o t) LedgerCircuitGates tc)
ledgerSetup TrustedSetup {..} ac = setupV
 where
  (omega, k1, k2) = getParams (Number.value @LedgerCircuitGates)
  plonkup = Plonkup omega k1 k2 ac g2_1 g1s
  setupV = setupVerify @(PlonkupTs (LedgerContractCompiledInput bi bo ud a i o t) LedgerCircuitGates tc) plonkup
