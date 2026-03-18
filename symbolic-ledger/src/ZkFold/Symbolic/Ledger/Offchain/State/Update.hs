{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Symbolic.Ledger.Offchain.State.Update (
  updateLedgerState,
) where

import Data.Function (($), (&))
import GHC.Generics ((:*:) (..), (:.:) (..))
import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq ((==))
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Data.Vector
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (BoolType (..), false, (||))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hashable (..), hash)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.Maybe (Maybe (..))
import ZkFold.Symbolic.Data.MerkleTree (MerkleTree)
import ZkFold.Symbolic.Data.MerkleTree qualified as MerkleTree
import ZkFold.Symbolic.WitnessContext (toWitnessContext)
import Prelude qualified as P

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Utils (replaceFirstMatchWith, replaceFirstMatchWith', unsafeToVector')
import ZkFold.Symbolic.Ledger.Validation.State (StateWitness (..))
import ZkFold.Symbolic.Ledger.Validation.Transaction (TransactionWitness (..))
import ZkFold.Symbolic.Ledger.Validation.TransactionBatch (TransactionBatchWitness (..))

-- | Update ledger state.
--
-- This function assumes that provided inputs are valid in the sense that say transaction outputs contain at least one ada, given UTxO set correctly corresponds to merkle tree, etc.. We can use @validateStateUpdate@ on top of this function to check if inputs are valid.
updateLedgerState
  :: forall bi bo ud a s n t context
   . SignatureState bi bo ud a context
  => SignatureTransactionBatch ud s n a t context
  => State ud a context
  -- ^ Previous state.
  -> MerkleTree ud context
  -- ^ Full Merkle tree corresponding to the previous state's UTxO root hash.
  -> Leaves ud (UTxO a context)
  -- ^ UTxO set (preimage of leaves of the merkle tree). It is assumed that it corresponds correctly to the merkle tree.
  -> (Vector bi :.: Output a) context
  -- ^ Bridged in outputs.
  -> TransactionBatch n a t context
  -- ^ Transaction batch.
  -> (Vector t :.: (Vector s :.: (PublicKey :*: EdDSAPoint :*: EdDSAScalarField))) context
  -- ^ Signature material for each transaction: per-signer (publicKey :*: rPoint :*: s).
  -> (State ud a :*: StateWitness bi bo ud a s n t :*: MerkleTree ud :*: (Leaves ud :.: UTxO a)) context
  -- ^ New state, witness, updated Merkle tree and UTxO set.
updateLedgerState previousState initialTree utxoSet bridgedInOutputs action sigMaterial =
  let
    nullOutput' = nullOutput @a @context
    nullUTxO' = nullUTxO @a @context
    nullUTxOHash' = nullUTxOHash @a @context
    newLen = previousState.sLength + one
    bridgeInHash :: HashSimple context
    bridgeInHash = newLen & hash & Base.hHash
    emptyBoVec :: (Vector bo :.: Output a) context
    emptyBoVec = Comp1 (P.pure nullOutput')

    txs = fromVector action.tbTransactions
    -- Compute bridged out outputs list.
    bridgedOutOutputs =
      let step acc tx =
            let outs = fromVector (unComp1 tx.outputs)
             in foldl'
                  ( \acc' (out :*: bout) ->
                      ifThenElse
                        bout
                        (Comp1 $ replaceFirstMatchWith (unComp1 acc') nullOutput' out)
                        acc'
                  )
                  acc
                  outs
       in foldl' step emptyBoVec txs

    biOutsList = fromVector (unComp1 bridgedInOutputs)
    -- Maintain a local preimage vector of UTxOs in parallel with the Merkle tree
    utxoPreimageInit = utxoSet
    -- Apply bridge-in outputs to the UTxO set and collect witness entries.
    stepBridgeIn (ix, entries, tree, pre) out =
      let entry = MerkleTree.search' (\(fe :: FieldElement e) -> fe == nullUTxOHash @a @e) tree
          utxo = UTxO {uRef = OutputRef {orTxId = bridgeInHash, orIndex = ix}, uOutput = out}
          utxoHash = hash utxo & Base.hHash
          tree' :*: gatedUtxo =
            ifThenElse
              (out == nullOutput')
              (tree :*: nullUTxO')
              (MerkleTree.replace (entry {MerkleTree.value = utxoHash}) tree :*: utxo)
          ix' = ix + one
          entries' = entry : entries
          pre' = replaceFirstMatchWith pre nullUTxO' gatedUtxo
       in (ix', entries', tree', pre')
    (_ixAfterBI, biEntriesRev, utxoAfterBridgeIn, utxoPreimageAfterBI) = foldl' stepBridgeIn (zero, [], initialTree, utxoPreimageInit) biOutsList
    swAddBridgeIn = Comp1 (unsafeToVector' @bi (P.reverse biEntriesRev))

    -- Build transaction witnesses and apply batch updates to UTxO tree
    sigsPerTx = fromVector (unComp1 sigMaterial)
    txsList = fromVector action.tbTransactions

    buildTx (tree, pre, witsAcc) (tx, sigs) =
      let
        txId' = txId tx & Base.hHash
        -- Inputs witnesses
        inRefs = fromVector (unComp1 tx.inputs)
        stepIn (insAcc, treeIn, preIn) ref =
          let
            -- Find UTxO by reference in evolving preimage set
            utxoSetList = fromVector preIn
            pick (found, picked) u =
              let isHere = u.uRef == ref
               in (isHere || found, ifThenElse isHere u picked)
            (_foundU, utxo) = foldl' pick (false, nullUTxO') utxoSetList
            utxoHash :: FieldElement context = hash utxo & Base.hHash
            utxoHashWC = toWitnessContext utxoHash
            me = fromJust $ MerkleTree.search (== utxoHashWC) treeIn
            treeIn' = MerkleTree.replace (me {MerkleTree.value = nullUTxOHash'}) treeIn
            preIn' = replaceFirstMatchWith' preIn (\u -> u.uRef == ref) nullUTxO'
           in
            ((me :*: utxo) : insAcc, treeIn', preIn')
        (insRev, treeAfterIns, preAfterIns) = foldl' stepIn ([], tree, pre) inRefs
        twInputs = Comp1 (unsafeToVector' @n (P.reverse insRev))
        twSignatures = sigs

        -- Outputs witnesses and apply outputs (skip bridge-outs)
        outs = fromVector (unComp1 tx.outputs)
        stepOut (outsAcc, outIx, treeOut, preOut) (out :*: bout) =
          let me = MerkleTree.search' (\(fe :: FieldElement e) -> fe == nullUTxOHash @a @e) treeOut
              utxo = UTxO {uRef = OutputRef {orTxId = txId', orIndex = outIx}, uOutput = out}
              utxoHash = hash utxo & Base.hHash
              treeOut' :*: gatedUtxo =
                ifThenElse
                  (bout || (out == nullOutput'))
                  (treeOut :*: nullUTxO')
                  (MerkleTree.replace (me {MerkleTree.value = utxoHash}) treeOut :*: utxo)
              preOut' = replaceFirstMatchWith preOut nullUTxO' gatedUtxo
           in (me : outsAcc, outIx + one, treeOut', preOut')
        (outsRev, _outIxEnd, treeAfterOuts, preAfterOuts) = foldl' stepOut ([], zero, treeAfterIns, preAfterIns) outs
        twOutputs = Comp1 (unsafeToVector' @n (P.reverse outsRev))
        tw = TransactionWitness {twSignatures, twInputs, twOutputs}
       in
        (treeAfterOuts, preAfterOuts, tw : witsAcc)

    (utxoFinal, utxoPreimageFinal, txWitsRev) = foldl' buildTx (utxoAfterBridgeIn, utxoPreimageAfterBI, []) (P.zip txsList sigsPerTx)
    tbwTransactions = Comp1 (unsafeToVector' @t (P.reverse txWitsRev))

    newState =
      State
        { sPreviousStateHash = hasher previousState
        , sUTxO = MerkleTree.mHash utxoFinal
        , sLength = newLen
        }
   in
    newState
      :*: StateWitness {swBridgeIn = bridgedInOutputs, swBridgeOut = bridgedOutOutputs, swAddBridgeIn, swTransactionBatch = TransactionBatchWitness {tbwTransactions}}
      :*: utxoFinal
      :*: Comp1 utxoPreimageFinal
