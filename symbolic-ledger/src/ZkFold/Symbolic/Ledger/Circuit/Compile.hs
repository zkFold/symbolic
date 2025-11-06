{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Symbolic.Ledger.Circuit.Compile (
  ledgerCircuit,
  ledgerSetup,
  ledgerProof,
) where

import Data.Type.Equality (type (~))
import Data.Word (Word8)
import GHC.Generics (Generic, Generic1, Par1, U1 (..), (:*:) (..))
import GHC.TypeNats (type (+), type (^))
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (
  BLS12_381_G1_CompressedPoint,
  BLS12_381_G1_JacobianPoint,
  BLS12_381_G2_JacobianPoint,
 )
import ZkFold.Algebra.EllipticCurve.Jubjub (Fq)
import ZkFold.Algebra.Number qualified as Number
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.ArithmeticCircuit
import ZkFold.FFI.Rust.Plonkup (rustPlonkupProve)
import ZkFold.Protocol.NonInteractiveProof (
  FromTranscript (..),
  ToTranscript (..),
 )
import ZkFold.Protocol.NonInteractiveProof as NP (
  NonInteractiveProof (..),
  TrustedSetup (..),
 )
import ZkFold.Protocol.Plonkup (Plonkup (..))
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))
import ZkFold.Protocol.Plonkup.Utils (getParams)
import ZkFold.Protocol.Plonkup.Witness (PlonkupWitnessInput (..))
import ZkFold.Symbolic.Class (BaseField)
import ZkFold.Symbolic.Compiler qualified as C
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Vec (Vec (..), runVec)
import ZkFold.Symbolic.Interpreter
import Prelude (($), (.))

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.State
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hash (..))
import ZkFold.Symbolic.Data.MerkleTree (MerkleTree(mHash))

data LedgerContractInput bi bo ud a i o t c = LedgerContractInput
  { lciPreviousState :: State bi bo ud a c
  , lciTransactionBatch :: TransactionBatch i o a t c
  , lciNewState :: State bi bo ud a c
  , lciStateWitness :: StateWitness bi bo ud a i o t c
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

type LedgerContractOutput =
  (FieldElement :*: FieldElement :*: FieldElement :*: FieldElement :*: FieldElement)
  :*: (FieldElement :*: FieldElement :*: FieldElement :*: FieldElement :*: FieldElement)
  :*: Bool

ledgerContract
  :: forall bi bo ud a i o t c
   . SignatureState bi bo ud a c
  => SignatureTransactionBatch ud i o a t c
  => LedgerContractInput bi bo ud a i o t c -> LedgerContractOutput c
ledgerContract LedgerContractInput {..} =
  ( sPreviousStateHash lciPreviousState
  :*: (mHash . sUTxO $ lciPreviousState)
  :*: sLength lciPreviousState
  :*: (hHash . sBridgeIn $ lciPreviousState)
  :*: (hHash . sBridgeOut $ lciPreviousState)
  ) :*:
  ( sPreviousStateHash lciNewState
  :*: (mHash . sUTxO $ lciNewState)
  :*: sLength lciNewState
  :*: (hHash . sBridgeIn $ lciNewState)
  :*: (hHash . sBridgeOut $ lciNewState)
  )
  :*: validateStateUpdate lciPreviousState lciTransactionBatch lciNewState lciStateWitness

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

type LedgerContractOutputLayout = (
  (Par1 :*: Par1 :*: Par1 :*: Par1 :*: Par1)
  :*: (Par1 :*: Par1 :*: Par1 :*: Par1 :*: Par1)
  :*: Par1
  )

type LedgerCircuit bi bo ud a i o t = ArithmeticCircuit Fq (LedgerContractCompiledInput bi bo ud a i o t) LedgerContractOutputLayout

ledgerCircuit
  :: forall bi bo ud a i o t c
   . SignatureState bi bo ud a c
  => SignatureTransactionBatch ud i o a t c
  => -- Since we are hardcoding @Fq@ at some places in this file, it is important that it is the same as the base field of the context.
  Fq ~ BaseField c
  => LedgerCircuit bi bo ud a i o t
ledgerCircuit = runVec $ C.compile @Fq ledgerContract

type PlonkupTs i n t = Plonkup i LedgerContractOutputLayout n BLS12_381_G1_JacobianPoint BLS12_381_G2_JacobianPoint t (PolyVec Fq)

type TranscriptConstraints ts =
  ( ToTranscript ts Word8
  , ToTranscript ts Fq
  , ToTranscript ts BLS12_381_G1_CompressedPoint
  , FromTranscript ts Fq
  )

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

ledgerProof
  :: forall tc bi bo ud a i o t c
   . (TranscriptConstraints tc, c ~ Interpreter Fq)
  => SignatureState bi bo ud a c
  => SignatureTransactionBatch ud i o a t c
  => TrustedSetup (LedgerCircuitGates + 6)
  -> PlonkupProverSecret BLS12_381_G1_JacobianPoint
  -> LedgerCircuit bi bo ud a i o t
  -> LedgerContractInput bi bo ud a i o t c
  -> Proof (PlonkupTs (LedgerContractCompiledInput bi bo ud a i o t) LedgerCircuitGates tc)
ledgerProof TrustedSetup {..} ps ac input = proof
 where
  witnessInputs :: (Layout (LedgerContractInput bi bo ud a i o t) (Order Fq)) Fq
  witnessInputs = runInterpreter $ arithmetize input

  paddedWitnessInputs :: LedgerContractCompiledInput bi bo ud a i o t Fq
  paddedWitnessInputs = (payload input :*: U1) :*: (witnessInputs :*: U1)

  (omega, k1, k2) = getParams (Number.value @LedgerCircuitGates)
  plonkup = Plonkup omega k1 k2 ac g2_1 g1s :: PlonkupTs (LedgerContractCompiledInput bi bo ud a i o t) LedgerCircuitGates tc
  setupP = setupProve @(PlonkupTs (LedgerContractCompiledInput bi bo ud a i o t) LedgerCircuitGates tc) plonkup
  witness =
    ( PlonkupWitnessInput @(LedgerContractCompiledInput bi bo ud a i o t) @BLS12_381_G1_JacobianPoint paddedWitnessInputs
    , ps
    )
  (proof, _) = rustPlonkupProve setupP witness
