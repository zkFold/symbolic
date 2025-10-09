{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Symbolic.Ledger.Offchain.State.Update (
  updateLedgerState,
) where

import Data.Function (($), (&))
import GHC.Generics ((:*:) (..), (:.:) (..))
import GHC.TypeNats (KnownNat)
import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq ((==))
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Data.Vector
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (BoolType (..), false, (&&), (||))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hashable (..), hash)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.Maybe (Maybe (..))
import ZkFold.Symbolic.Data.MerkleTree qualified as MerkleTree
import ZkFold.Symbolic.WitnessContext (toWitnessContext)
import Prelude qualified as P

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.State (StateWitness (..))
import ZkFold.Symbolic.Ledger.Validation.Transaction (TransactionWitness (..))
import ZkFold.Symbolic.Ledger.Validation.TransactionBatch (TransactionBatchWitness (..))

-- TODO: Should this function also check if inputs are valid in the sense, that say outputs contain at least one ada? We could return "Maybe" result.

-- | Update ledger state.
updateLedgerState
  :: forall bi bo ud a i o t context
   . SignatureState bi bo ud a context
  => SignatureTransactionBatch ud i o a t context
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
updateLedgerState previousState utxoSet bridgedInOutputs action sigMaterial =
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

    -- Apply bridge-in outputs to UTxO tree by replacing null leaves and collect witness entries
    biOutsList = fromVector (unComp1 bridgedInOutputs)
    stepBridgeIn (ix, entries, tree) out =
      let entry = MerkleTree.search' (\(fe :: FieldElement e) -> fe == nullUTxOHash @a @e) tree
          tree' =
            ifThenElse
              (out == nullOutput @a @context)
              tree
              ( let utxo = UTxO {uRef = OutputRef {orTxId = bridgeInHash, orIndex = ix}, uOutput = out}
                    utxoHash = hash utxo & Base.hHash
                 in MerkleTree.replace (entry {MerkleTree.value = utxoHash}) tree
              )
          ix' = ix + one
          entries' = entry : entries
       in (ix', entries', tree')
    (_ixAfterBI, biEntriesRev, utxoAfterBridgeIn) = foldl' stepBridgeIn (zero, [], previousState.sUTxO) biOutsList
    swAddBridgeIn = Comp1 (unsafeToVector' @bi (P.reverse biEntriesRev))

    -- Build transaction witnesses and apply batch updates to UTxO tree
    sigsPerTx = fromVector (unComp1 sigMaterial)
    txsList = fromVector action.tbTransactions

    buildTx (tree, witsAcc) (tx, sigs) =
      let
        txId' = txId tx & Base.hHash
        -- Inputs witnesses
        inRefs = fromVector (unComp1 tx.inputs)
        sigsList = fromVector (unComp1 sigs)
        stepIn (insAcc, treeIn) (ref, rPoint :*: s :*: publicKey) =
          let
            -- Find UTxO by reference in provided set
            utxoSetList = fromVector utxoSet
            pick (found, picked) u =
              let isHere = u.uRef == ref
               in (isHere || found, ifThenElse isHere u picked)
            (_foundU, utxo) = foldl' pick (false, nullUTxO @a @context) utxoSetList
            utxoHash :: FieldElement context = hash utxo & Base.hHash
            utxoHashWC = toWitnessContext utxoHash
            me = fromJust $ MerkleTree.search (== utxoHashWC) treeIn
            treeIn' = MerkleTree.replace (me {MerkleTree.value = nullUTxOHash @a @context}) treeIn
           in
            ((me :*: utxo :*: rPoint :*: s :*: publicKey) : insAcc, treeIn')
        (insRev, treeAfterIns) = foldl' stepIn ([], tree) (P.zip inRefs sigsList)
        twInputs = Comp1 (unsafeToVector' @i (P.reverse insRev))

        -- Outputs witnesses and apply outputs (skip bridge-outs)
        outs = fromVector (unComp1 tx.outputs)
        stepOut (outsAcc, outIx, treeOut) (out :*: bout) =
          let me = MerkleTree.search' (\(fe :: FieldElement e) -> fe == nullUTxOHash @a @e) treeOut
              treeOut' =
                ifThenElse
                  bout
                  treeOut
                  ( let utxo = UTxO {uRef = OutputRef {orTxId = txId', orIndex = outIx}, uOutput = out}
                        utxoHash = hash utxo & Base.hHash
                     in MerkleTree.replace (me {MerkleTree.value = utxoHash}) treeOut
                  )
           in (me : outsAcc, outIx + one, treeOut')
        (outsRev, _outIxEnd, treeAfterOuts) = foldl' stepOut ([], zero, treeAfterIns) outs
        twOutputs = Comp1 (unsafeToVector' @o (P.reverse outsRev))
        tw = TransactionWitness {twInputs, twOutputs}
       in
        (treeAfterOuts, tw : witsAcc)

    (utxoFinal, txWitsRev) = foldl' buildTx (utxoAfterBridgeIn, []) (P.zip txsList sigsPerTx)
    tbwTransactions = Comp1 (unsafeToVector' @t (P.reverse txWitsRev))

    newState =
      State
        { sPreviousStateHash = hasher previousState
        , sUTxO = utxoFinal
        , sLength = newLen
        , sBridgeIn = hash bridgedInOutputs
        , sBridgeOut = hash bridgedOutOutputs
        }
   in
    newState
      :*: StateWitness {swAddBridgeIn, swTransactionBatch = TransactionBatchWitness {tbwTransactions}}

-- | Unsafe conversion from list to vector. This differs from `unsafeToVector` in that it throws an error if the list is not of the correct length.
unsafeToVector' :: forall size a. KnownNat size => [a] -> Vector size a
unsafeToVector' as = case toVector as of
  P.Nothing -> P.error "unsafeToVector': toVector failed"
  P.Just v -> v
