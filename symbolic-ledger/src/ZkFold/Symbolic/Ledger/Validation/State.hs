{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Symbolic.Ledger.Validation.State (
  validateStateUpdate,
) where

import GHC.Generics ((:*:) (..))
import ZkFold.Data.Eq ((==))
import ZkFold.Symbolic.Data.Bool (Bool, BoolType ((&&)))
import ZkFold.Symbolic.Data.Hash (Hashable (..), preimage)
import qualified Prelude as P

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.TransactionBatch (validateTransactionBatch)

validateStateUpdate
  :: forall context bi bo t users
   . SignatureState context bi bo users
  => SignatureTransactionBatch context t
  => State bi bo users context
  -- ^ Previous state.
  -> TransactionBatch t context
  -- ^ The "action" that is applied to the state.
  -> State bi bo users context
  -- ^ New state.
  -> Bool context
validateStateUpdate previousState action newState =
  let
    -- Bridged in assets are added to the account info. This represents our starting point of account info.
    merkleTreeWithBridgedInAssets = addBridgedInAssets previousState.sAccountInfo (preimage newState.sBridgeIn)
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
