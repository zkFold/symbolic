-- TODO: Is this language extension needed?
{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Symbolic.Ledger.Circuit.Compile (
  ledgerCircuit,
) where

-- TODO: Refine import from SmartWallet.

-- import GHC.TypeNats (type (^))

import Data.Type.Equality (type (~))
import GHC.Generics (Generic, Generic1, Par1, U1, (:*:))
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Jubjub (Fq)
import ZkFold.ArithmeticCircuit
import ZkFold.Symbolic.Class (BaseField)
import ZkFold.Symbolic.Compiler qualified as C
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Vec (Vec (..), runVec)
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
-- type LedgerCircuitGates = 2 ^ 18

type LedgerContractInputLayout bi bo ud a i o t =
  Layout
    (LedgerContractInput bi bo ud a i o t :*: U1)
    (Order Fq)

type LedgerContractInputPayload bi bo ud a i o t =
  Payload
    (LedgerContractInput bi bo ud a i o t :*: U1)
    (Order Fq)

-- Payload (LedgerContractInput bi bo ud a i o t) (Order (BaseField c))

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
