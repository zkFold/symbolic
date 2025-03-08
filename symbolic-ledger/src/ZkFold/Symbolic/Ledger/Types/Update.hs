{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Ledger.Types.Update where

import           Prelude                                hiding (Bool, Eq, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Data.Bool              (Bool)
import           ZkFold.Symbolic.Data.Class             (SymbolicData (..), SymbolicOutput)
import           ZkFold.Symbolic.Data.List              (List, emptyList)
import           ZkFold.Symbolic.Ledger.Types.Address   (Address)
import           ZkFold.Symbolic.Ledger.Types.Contract  (ContractData)
import           ZkFold.Symbolic.Ledger.Types.Hash      (Hash)
import           ZkFold.Symbolic.Ledger.Types.Input     (Input)
import           ZkFold.Symbolic.Ledger.Types.Output    (Output)
import           ZkFold.Symbolic.Ledger.Types.OutputRef (TransactionId)

-- TODO: make type AddressIndex = UInt 40 Auto
data AddressIndex context

getAddressIndex :: Input context -> AddressIndex context
getAddressIndex = undefined

-- TODO: Add contract public data to the update.

data Update context = Update
  { updateOnlineTxsRoot   :: Hash context
    -- ^ the Merkle tree root of the TxId list of transactions that contains online transactions.
  , updateHashPrevious    :: Hash context
    -- ^ hash of previous update
  , updateNewAssignments  :: List context (Address context, AddressIndex context)
    -- ^ the map from addresses into assigned indices. Only new assignments.
  , updateSpentOutputs    :: List context (AddressIndex context, Bool context)
    -- ^ the map from address indices into boolean values.
    -- The keys are all indices of addresses from which an output was spent in the update.
    -- The boolean values indicate whether the transactions
    -- that spent outputs from a particular address should be included in the Update.
  , updateTransactions    :: List context (AddressIndex context, TransactionId context)
    -- ^ the map from address indices into transaction id lists.
    -- Contains offline transactions.
  , updateTransactionData :: List context (AddressIndex context, ContractData context)
    -- ^ the map from address indices into the accumulated public data items.
    -- Only non-empty public data.
  , updateIndicesReleased :: List context (AddressIndex context)
    -- ^ the list of indices that are being released after the update.
  , updateBridgedOutputs  :: List context (Address context, List context (Output context))
    -- ^ outputs that were bridged into the ledger.
    -- Note that there are two ways to create transaction inputs in the ledger:
    -- as transaction outputs of ledger transactions and as bridged outputs.
    -- We will need to associate some `InputRef` with the latter, too.
  , updateBridgedInputs   :: List context (AddressIndex context, List context (Input context))
    -- ^ inputs that were bridged out of the ledger.
    -- In order to bridge out of the ledger,
    -- a user proves that there is a transaction in this update
    -- that produced this Input.
  }

emptyUpdate ::
     SymbolicOutput (Hash context)
  => SymbolicOutput (AddressIndex context)
  => SymbolicOutput (ContractData context)
  => SymbolicData (Input context)
  => SymbolicOutput (Output context)
  => Context (Hash context) ~ context
  => Context (AddressIndex context) ~ context
  => Context (ContractData context) ~ context
  => Context (Input context) ~ context
  => Context (Output context) ~ context
  => Hash context
  -> Update context
emptyUpdate prev = Update
  { updateOnlineTxsRoot = merkleTreeRoot emptyList
  , updateHashPrevious = prev
  , updateNewAssignments = emptyList
  , updateSpentOutputs = emptyList
  , updateTransactions = emptyList
  , updateTransactionData = emptyList
  , updateIndicesReleased = emptyList
  , updateBridgedOutputs = emptyList
  , updateBridgedInputs = emptyList
  }

type UpdateChain context = List context (Update context)

-- | Get the block of the `Update`
updateId :: Update context -> Hash context
updateId = undefined

merkleTreeRoot :: List context (AddressIndex context, TransactionId context) -> Hash context
merkleTreeRoot = undefined

merkleTreeAdd ::
     Hash context
  -> List context (Hash context)
  -> (Hash context, List context (Hash context))
merkleTreeAdd = undefined

merkleTreeEmpty :: List context (Hash context)
merkleTreeEmpty = undefined
