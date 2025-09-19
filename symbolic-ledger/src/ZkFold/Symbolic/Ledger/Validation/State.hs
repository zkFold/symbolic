{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Symbolic.Ledger.Validation.State (
  validateStateUpdate,
) where

import Data.Functor (Functor (..))
import GHC.Generics ((:*:) (..), (:.:) (..))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq ((==))
import ZkFold.Data.Vector (Vector, Zip (..))
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.Hash (Hashable (..), preimage)
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry, MerklePath, MerkleTree)
import qualified Prelude as P

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.TransactionBatch (validateTransactionBatch)

data StateWitness bi bo users ad context = StateWitness
  { swBridgeIn :: (Vector bi :.: (MerkleTree ad :*: MerkleEntry ad :*: AssetValue :*: MerklePath ad)) context
  -- ^
  -- 1. User must have given correct merkle tree, we check it by computing root hash.
  }

-- TODO: Do we need to check if the root hash given by `MerkleTree` in witness is consistent with it's leaves?

validateStateUpdate
  :: forall context bi bo t users ad
   . SignatureState context bi bo users
  => SignatureTransactionBatch context t
  => State bi bo users context
  -- ^ Previous state.
  -> TransactionBatch t context
  -- ^ The "action" that is applied to the state.
  -> State bi bo users context
  -- ^ New state.
  -> StateWitness bi bo users ad context
  -- ^ Witness for the state.
  -> Bool context
validateStateUpdate previousState action newState sw =
  let
    bridgeInAssets = preimage newState.sBridgeIn
    bridgedInAssetsWithTree = zipWith (:*:) (unComp1 bridgeInAssets) (unComp1 sw.swBridgeIn)
    (Comp1 accountInfoOldState) = preimage previousState.sAccountInfo
    accountInfoWithBridgedInAssets' =
      foldl'
        ( \acc ((address :*: assetValue) :*: _) ->
            let
              addressExists = foldl' (\found ((addr :*: _ :*: _)) -> found || addr == address) false acc
             in
              ifThenElse
                addressExists
                P.undefined
                P.undefined
        )
        accountInfoOldState
        bridgedInAssetsWithTree
    accountInfoWithBridgedInAssets =
      fmap
        ( \(address :*: nonce :*: hash) ->
            let x = foldl' (\found ((addr :*: av) :*: _) -> found || addr == address) (false :: Bool context) bridgedInAssetsWithTree
             in ifThenElse
                  (true :: Bool context)
                  (address :*: nonce :*: hash)
                  (address :*: nonce :*: hash)
        )
        accountInfoOldState
    -- Bridged in assets are added to the account info. This represents our starting point of account info.
    merkleTreeWithBridgedInAssets = addBridgedInAssets previousState.sAccountInfo (bridgeInAssets)
    -- To verify validity of bridged out assets, we use following approach:
    --
    -- 1. If a transaction bridges out an asset, we see it is included in bridge out list.
    -- 2. When traversing transactions in batch, we maintain two merkle trees. If a transaction bridges out, we subtract it from one but not from the other. In the end, we subtract "bridgedOutAssets" from the other and check if both are equal.
    bridgedOutAssets = preimage newState.sBridgeOut
    (isBatchValid :*: newAi :*: newAiWithoutBridgedOut) =
      validateTransactionBatch merkleTreeWithBridgedInAssets bridgedOutAssets action
   in
    -- New state correctly links to the previous state.
    newState.sPreviousStateHash
      == hasher previousState
      -- See above note on how we verify for bridged out assets.
      && subtractBridgedOutAssets bridgedOutAssets newAiWithoutBridgedOut
      == newAi
      -- New account info is the same as the one in the new state.
      && newAi
      == newState.sAccountInfo
      -- Batch is valid.
      && isBatchValid

addBridgedInAssets = P.undefined

subtractBridgedOutAssets = P.undefined
