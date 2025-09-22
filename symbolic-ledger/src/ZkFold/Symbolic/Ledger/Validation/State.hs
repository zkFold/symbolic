{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Symbolic.Ledger.Validation.State (
  validateStateUpdate,
  StateWitness (..),
) where

import Data.Function ((&))
import GHC.Generics ((:*:) (..), (:.:) (..))
import ZkFold.Algebra.Class (FromConstant (..), MultiplicativeMonoid (..), Zero (..), (+))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq (Eq (..), (==))
import ZkFold.Data.Ord ((>=))
import ZkFold.Data.Vector (Vector, Zip (..))
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.Hash (Hashable (..), hash, preimage)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry)
import ZkFold.Symbolic.Data.MerkleTree qualified as MerkleTree
import Prelude qualified as P

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.Transaction (outputHasAtLeastOneAda)
import ZkFold.Symbolic.Ledger.Validation.TransactionBatch (TransactionBatchWitness, validateTransactionBatch)

{- Note [State validation]

For validating transactions, we should check:

\* Inputs come from UTxO set. Inputs list does not include the "null" UTxO.
\* We subtract inputs UTxOs from the UTxO set.
\* Outputs are added to the UTxO set if they are not bridge out outputs.
\* We require signature from addresses corresponding to spent UTxOs.
\* Bridged out outputs are checked to be same as in bridge out list.
\* Outputs must have at least one ada.
\* Transaction is balanced.

For validating batch, we simply apply transaction validation check iteratively.

For validating state, we check following:

\* Previous state hash is correctly set.
\* New UTxO state is properly computed. For it, we first added UTxOs to the old state corresponding to bridged in assets and then update this set by folding over transactions.
\* Bridge in outputs have at least one ada.
\* Transaction batch is valid.
\* Length is incremented by one.
\* Bridged out list is correctly computed.
-}

data StateWitness bi bo ud a i o t context = StateWitness
  { swAddBridgeIn :: (Vector bi :.: MerkleEntry ud) context
  , swTransactionBatch :: (TransactionBatchWitness ud i o a t) context
  }

validateStateUpdate
  :: forall bi bo ud a i o t context
   . SignatureState bi bo ud a context
  => SignatureTransactionBatch ud i o a t context
  => State bi bo ud a context
  -- ^ Previous state.
  -> TransactionBatch i o a t context
  -- ^ The "action" that is applied to the state.
  -> State bi bo ud a context
  -- ^ New state.
  -> StateWitness bi bo ud a i o t context
  -- ^ Witness for the state.
  -> Bool context
validateStateUpdate previousState action newState sw =
  let
    initialUTxOTree = previousState.sUTxO
    bridgeInAssets = preimage newState.sBridgeIn
    bridgedInAssetsWithWitness = zipWith (:*:) (unComp1 bridgeInAssets) (unComp1 sw.swAddBridgeIn)
    -- TODO: Do we need to check if merkle entry's given are all unique? Perhaps not as when it's updated, the `contains` check will fail.

    bridgeInHash = newState.sLength & hash & Base.hHash
    (_ :*: isWitBridgeInValid :*: utxoTreeWithBridgeIn) =
      foldl'
        ( \(ix :*: isValidAcc :*: acc) ((output :*: merkleEntry)) ->
            let nullUTxOHash' = nullUTxOHash @a @context
                isValid' =
                  isValidAcc
                    && ifThenElse
                      (utxoHash == nullUTxOHash')
                      true
                      ( (acc `MerkleTree.contains` merkleEntry)
                          && (merkleEntry.value == nullUTxOHash')
                          && outputHasAtLeastOneAda output
                      )
                utxo = UTxO {uRef = OutputRef {orTxId = bridgeInHash, orIndex = ix}, uOutput = output}
                utxoHash = hash utxo & Base.hHash
             in ( (ix + one)
                    :*: isValid'
                    :*: ifThenElse
                      (isValid' && (utxoHash /= nullUTxOHash'))
                      ( MerkleTree.replace
                          ( merkleEntry
                              { MerkleTree.value = utxoHash
                              }
                          )
                          acc
                      )
                      acc
                )
        )
        (zero :*: true :*: initialUTxOTree)
        bridgedInAssetsWithWitness
    bridgedOutOutputs = preimage newState.sBridgeOut
    (isBatchValid :*: utxoTree) = validateTransactionBatch sw.swTransactionBatch utxoTreeWithBridgeIn bridgedOutOutputs action
   in
    -- New state correctly links to the previous state.
    newState.sPreviousStateHash
      == hasher previousState
      && newState.sLength
      == previousState.sLength
      + one -- TODO: Confirm if this is the correct way to increment the length.
      && isWitBridgeInValid
      && isBatchValid
      && utxoTree
      == newState.sUTxO
