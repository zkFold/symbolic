{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module ZkFold.Symbolic.Ledger.Offchain.State.Update (

) where

import qualified Prelude as P
import ZkFold.Symbolic.Ledger.Types
import GHC.Generics ((:*:) (..), (:.:) (..))
import GHC.TypeNats (KnownNat)
import ZkFold.Symbolic.Ledger.Validation.State (StateWitness)
import Data.Function ((&))
import ZkFold.Data.Vector
import ZkFold.Symbolic.Data.Hash (Hashable(..), hash)
import qualified ZkFold.Symbolic.Data.Hash as Base
import ZkFold.Algebra.Class
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Symbolic.Data.Bool (true, false, (||), (&&), BoolType (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Prelude (foldl')
import ZkFold.Data.Eq ((==))
import qualified ZkFold.Symbolic.Data.MerkleTree as MerkleTree
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry, KnownMerkleTree)

-- TODO: Should this function also check if inputs are valid in the sense, that say outputs contain at least one ada? We could return "Maybe" result.

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
    newLen = previousState.sLength + one
    bridgeInHash :: HashSimple context
    bridgeInHash = newLen & hash & Base.hHash
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

    -- Apply bridge-in outputs to UTxO tree by replacing null leaves
    biOuts = unComp1 bridgedInOutputs
    stepBridgeIn (ix :*: tree) out =
      let isNullOut = out == nullOutput @a @context
          entry = MerkleTree.search' (\(fe :: FieldElement e) -> fe == nullUTxOHash @a @e) tree
          tree' =
            ifThenElse
              isNullOut
              tree
              ( let utxo = UTxO {uRef = OutputRef {orTxId = bridgeInHash, orIndex = ix}, uOutput = out}
                    utxoHash = hash utxo & Base.hHash
                 in MerkleTree.replace (entry {MerkleTree.value = utxoHash}) tree
              )
       in (ix + one) :*: tree'
    (_ :*: utxoAfterBridgeIn) = foldl' stepBridgeIn (zero :*: previousState.sUTxO) (fromVector biOuts)

    newState = State {
      sPreviousStateHash = hasher previousState,
      sUTxO = utxoAfterBridgeIn,
      sLength = newLen,
      sBridgeIn = hash bridgedInOutputs,
      sBridgeOut = hash bridgedOutOutputs
    }
    
    
    in newState :*: P.undefined
  