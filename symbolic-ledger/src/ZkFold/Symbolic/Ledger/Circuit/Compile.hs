-- TODO: Is this language extension needed?
{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Symbolic.Ledger.Circuit.Compile (

) where

-- TODO: Refine import from SmartWallet.

import GHC.Generics (Generic, Generic1, Par1, U1, (:*:))
import GHC.TypeNats (type (^))
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Jubjub (Fq)
import ZkFold.ArithmeticCircuit
import ZkFold.Symbolic.Class
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
type LedgerCircuitGates = 2 ^ 18

type LedgerContractInputLayout bi bo ud a i o t c = Layout (LedgerContractInput bi bo ud a i o t :*: U1) (52435875175126190479447740508185965837690552500527637822603658699938581184513)

type LedgerContractInputPayload bi bo ud a i o t c =
             Payload
                    (LedgerContractInput bi bo ud a i o t :*: U1)
          (52435875175126190479447740508185965837690552500527637822603658699938581184513)
  -- Payload (LedgerContractInput bi bo ud a i o t) (Order (BaseField c))

type LedgerContractCompiledInput bi bo ud a i o t c =
  (LedgerContractInputPayload bi bo ud a i o t c) :*: (LedgerContractInputLayout bi bo ud a i o t c)

type LedgerCircuit bi bo ud a i o t c = ArithmeticCircuit Fq (LedgerContractCompiledInput bi bo ud a i o t c) Par1

ledgerCircuit
  :: forall bi bo ud a i o t c
   . SignatureState bi bo ud a c => SignatureTransactionBatch ud i o a t c => LedgerCircuit bi bo ud a i o t c
ledgerCircuit = runVec $ C.compile @Fq ledgerContract
