{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImpredicativeTypes #-}
module ZkFold.Symbolic.Ledger.Offchain.State.Update (

) where

import qualified Prelude as P
import ZkFold.Symbolic.Ledger.Types
import GHC.Generics ((:*:) (..), (:.:))
import ZkFold.Symbolic.Ledger.Validation.State (StateWitness)
import ZkFold.Data.Vector
import ZkFold.Symbolic.Data.Hash (Hashable(..), hash)
import ZkFold.Algebra.Class
import ZkFold.Data.MerkleTree (Leaves)

-- | Update ledger state.
updateLedgerState
  :: forall bi bo ud a i o t context.
  SignatureState bi bo ud a context
  => SignatureTransactionBatch ud i o a t context
  => State bi bo ud a context 
  -- ^ Previous state.
  -> Leaves ud (UTxO a context)
  -- ^ UTxO set (preimage of leaves of the merkle tree). It is assumed that it corresponds correctly to the previous state's UTxO set
  -> (Vector bi :.: Output a) context
  -- ^ Bridged in outputs.
  -> TransactionBatch i o a t context
  -- ^ Transaction batch.
  -> (State bi bo ud a :*: StateWitness bi bo ud a i o t) context
  -- ^ New state and witness.
updateLedgerState previousState utxoSet bridgedInOutputs action = 
  
  let 
    newState = State {
      sPreviousStateHash = hasher previousState,
      sUTxO = P.undefined,
      sLength = previousState.sLength + one,
      sBridgeIn = hash bridgedInOutputs,
      sBridgeOut = P.undefined
    }
    
    
    in newState :*: P.undefined
  