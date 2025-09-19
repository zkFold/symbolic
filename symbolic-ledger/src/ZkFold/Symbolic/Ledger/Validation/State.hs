{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Symbolic.Ledger.Validation.State (
  validateStateUpdate,
) where

import Data.Function ((&))
import Data.Functor (Functor (..))
import GHC.Generics ((:*:) (..), (:.:) (..))
import ZkFold.Algebra.Class (MultiplicativeMonoid (..), Zero (..), (+))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq (Eq (..), (==))
import ZkFold.Data.Vector (Vector, Zip (..))
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.Hash (Hashable (..), hash, preimage)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry, MerklePath, MerkleTree, replace)
import ZkFold.Symbolic.Data.MerkleTree qualified as MerkleTree
import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.TransactionBatch (validateTransactionBatch)
import Prelude qualified as P

{- Note [State validation]

For validating transactions, we should check:

\* Inputs come from UTxO set. Inputs list does not include the "null" UTxO.
\* We subtract inputs UTxOs from the UTxO set.
\* Outputs are added to the UTxO set if they are not bridge out outputs.
\* We require signature from addresses corresponding to spent UTxOs.
\* Bridged out outputs are checked to be same as in bridge out list.

For validating batch, we simply apply transaction validation check iteratively.

For validating state, we check following:

\* Previous state hash is correctly set.
\* New UTxO state is properly computed. For it, we first added UTxOs to the old state corresponding to bridged in assets and then update this set by folding over transactions.
\* Transaction batch is valid.
\* Length is incremented by one.
\* Bridged out list is correctly computed.
-}

data StateWitness bi bo ud a context = StateWitness
  { swAddBridgeIn :: (Vector bi :.: (MerkleEntry ud)) context
  }

validateStateUpdate
  :: forall bi bo ud a i o t context
   . SignatureState bi bo ud a context
  => SignatureTransactionBatch i o a t context
  => State bi bo ud a context
  -- ^ Previous state.
  -> TransactionBatch i o a t context
  -- ^ The "action" that is applied to the state.
  -> State bi bo ud a context
  -- ^ New state.
  -> StateWitness bi bo ud a context
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
            let isValid' = isValidAcc && (acc `MerkleTree.contains` merkleEntry) && (merkleEntry.value == (nullUTxOHash @a @context))
                utxo = UTxO {uRef = OutputRef {orTxId = bridgeInHash, orIndex = ix}, uOutput = output}
                utxoHash = hash utxo & Base.hHash
             in ( (ix + one)
                    :*: isValid'
                    :*: ifThenElse
                      (isValid' && (utxoHash /= nullUTxOHash @a @context))
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
   in
    -- bridgeInAssets = preimage newState.sBridgeIn
    -- bridgedInAssetsWithTree = zipWith (:*:) (unComp1 bridgeInAssets) (unComp1 sw.swBridgeIn)
    -- (Comp1 accountInfoOldState) = preimage previousState.sAccountInfo
    -- accountInfoWithBridgedInAssets' =
    --   foldl'
    --     ( \acc ((address :*: assetValue) :*: _) ->
    --         let
    --           addressExists = foldl' (\found ((addr :*: _ :*: _)) -> found || addr == address) false acc
    --          in
    --           ifThenElse
    --             addressExists
    --             P.undefined
    --             P.undefined
    --     )
    --     accountInfoOldState
    --     bridgedInAssetsWithTree
    -- accountInfoWithBridgedInAssets =
    --   fmap
    --     ( \(address :*: nonce :*: hash) ->
    --         let x = foldl' (\found ((addr :*: av) :*: _) -> found || addr == address) (false :: Bool context) bridgedInAssetsWithTree
    --          in ifThenElse
    --               (true :: Bool context)
    --               (address :*: nonce :*: hash)
    --               (address :*: nonce :*: hash)
    --     )
    --     accountInfoOldState
    -- -- Bridged in assets are added to the account info. This represents our starting point of account info.
    -- merkleTreeWithBridgedInAssets = addBridgedInAssets previousState.sAccountInfo (bridgeInAssets)
    -- -- To verify validity of bridged out assets, we use following approach:
    -- --
    -- -- 1. If a transaction bridges out an asset, we see it is included in bridge out list.
    -- -- 2. When traversing transactions in batch, we maintain two merkle trees. If a transaction bridges out, we subtract it from one but not from the other. In the end, we subtract "bridgedOutAssets" from the other and check if both are equal.
    -- bridgedOutAssets = preimage newState.sBridgeOut
    -- (isBatchValid :*: newAi :*: newAiWithoutBridgedOut) =
    --   validateTransactionBatch merkleTreeWithBridgedInAssets bridgedOutAssets action

    -- New state correctly links to the previous state.
    newState.sPreviousStateHash
      == hasher previousState
      && newState.sLength
      == previousState.sLength
      + one -- TODO: Confirm if this is the correct way to increment the length.
      && isWitBridgeInValid
      && P.undefined

--   -- See above note on how we verify for bridged out assets.
--   && subtractBridgedOutAssets bridgedOutAssets newAiWithoutBridgedOut
--   == newAi
--   -- New account info is the same as the one in the new state.
--   && newAi
--   == newState.sAccountInfo
--   -- Batch is valid.
--   && isBatchValid

addBridgedInAssets = P.undefined

subtractBridgedOutAssets = P.undefined
