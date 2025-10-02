{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImpredicativeTypes #-}
module ZkFold.Symbolic.Ledger.Offchain.State.Update (

) where

import qualified Prelude as P
import ZkFold.Symbolic.Ledger.Types
import GHC.Generics ((:*:) (..), (:.:) (..))
import GHC.TypeNats (KnownNat)
import ZkFold.Symbolic.Ledger.Validation.State (StateWitness)
import ZkFold.Data.Vector
import ZkFold.Symbolic.Data.Hash (Hashable(..), hash)
import ZkFold.Algebra.Class
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Symbolic.Data.Bool (true, false, (||), (&&), BoolType (..))
import ZkFold.Prelude (foldl')
import ZkFold.Data.Eq ((==))

-- | Update ledger state.
updateLedgerState
  :: forall bi bo ud a i o t context.
  SignatureState bi bo ud a context
  => SignatureTransactionBatch ud i o a t context
  => KnownNat bo
  => State bi bo ud a context 
  -- ^ Previous state.
  -> Leaves ud (UTxO a context)
  -- ^ UTxO set (preimage of leaves of the merkle tree). It is assumed that it corresponds correctly to the previous state's UTxO set
  -> (Vector bi :.: Output a) context
  -- ^ Bridged in outputs.
  -> TransactionBatch i o a t context
  -- ^ Transaction batch.
  -> (Vector t :.: (Vector i :.: (EdDSAPoint :*: EdDSAScalarField :*: EdDSAPoint))) context
  -- ^ Signature material for each transaction input: (rPoint :*: s :*: publicKey).
  -> (State bi bo ud a :*: StateWitness bi bo ud a i o t) context
  -- ^ New state and witness.
updateLedgerState previousState utxoSet bridgedInOutputs action _sigMaterial = 
  
  let 
    emptyBoVec :: (Vector bo :.: Output a) context
    emptyBoVec = Comp1 (P.pure (nullOutput @a @context))

    insertFirstNull :: (Vector bo :.: Output a) context -> Output a context -> (Vector bo :.: Output a) context
    insertFirstNull acc out =
      let v = unComp1 acc
          isEmpty = (\x -> x == nullOutput @a @context) P.<$> v
          prefixUsed = scanl (||) false isEmpty
          usedBefore = take @bo prefixUsed
          -- We want only one entry inside `shouldIns` to be true.
          shouldIns = zipWith (\u e -> not u && e) usedBefore isEmpty
          v' = mapWithIx (\ix old -> ifThenElse (shouldIns !! ix) out old) v
       in Comp1 v'

    txs = fromVector action.tbTransactions
    bridgedOutOutputs =
      let step acc tx =
            let outs = fromVector (unComp1 tx.outputs)
             in foldl'
                  (\acc' (out :*: bout) -> ifThenElse bout (insertFirstNull acc' out) acc')
                  acc
                  outs
       in foldl' step emptyBoVec txs

    newState = State {
      sPreviousStateHash = hasher previousState,
      sUTxO = P.undefined,
      sLength = previousState.sLength + one,
      sBridgeIn = hash bridgedInOutputs,
      sBridgeOut = hash bridgedOutOutputs
    }
    
    
    in newState :*: P.undefined
  