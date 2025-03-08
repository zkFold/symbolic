module ZkFold.Symbolic.Ledger.Validation.Input where

import           Prelude                                  hiding (Bool, Eq, Maybe, all, any, concat, filter, head, init,
                                                           last, length, maybe, splitAt, tail, (!!), (&&), (*), (+),
                                                           (++), (/=), (==))

import           ZkFold.Symbolic.Data.Bool                (Bool, all, any, (&&))
import           ZkFold.Symbolic.Data.Eq                  (Eq (..))
import           ZkFold.Symbolic.Data.List
import           ZkFold.Symbolic.Ledger.Types
import           ZkFold.Symbolic.Ledger.Validation.Common (updateChainIsValid)

-- | Witness data that is required to prove the validity of an input.
data InputWitness context = InputWitness
    { inputWitnessTx :: Transaction context
    , inputWitnessDataLeastRecent :: InputWitnessDatum context
    , inputWitnessDataRest :: List context (Update context, List context (AddressIndex context, TransactionId context))
    , inputWitnessAllTxs :: List context (Transaction context)
      -- ^ all transactions which spend from the address of the given input
    }

data InputWitnessDatum context = InputWitnessDatum
    { inputWitnessUpdate     :: Update context
    , inputWitnessOnlineTxs  :: List context (AddressIndex context, Transaction context)
    , inputWitnessOfflineTxs :: List context (Transaction context)
    }

-- | Checks if the input existed.
inputExisted ::
       Signature context
    => Hash context
    -- ^ The id of the current block.
    -> Input context
    -- ^ The transaction input to check.
    -> InputWitness context
    -- ^ The witness data for the input.
    -> Bool context
inputExisted blockId i witness =
    let wUpdates = (fst <$> inputWitnessDataRest witness)
          ++ singleton (inputWitnessUpdate (inputWitnessDataLeastRecent witness))
        updMostRecent = head (fst <$> inputWitnessDataRest witness)
        updLeastRecentTxs = inputWitnessDataLeastRecent witness
        onAndOfflineTxs = (snd <$> inputWitnessOnlineTxs updLeastRecentTxs)
          ++ inputWitnessOfflineTxs updLeastRecentTxs

    in updateChainIsValid wUpdates
    -- ^ The update chain is valid
    && updateId updMostRecent == blockId
    -- ^ The most recent update is the current block
    && any (== txId (inputWitnessTx witness)) (txId <$> onAndOfflineTxs)
    -- ^ The transaction is included in the least recent update in the chain
    && refId (txiOutputRef i) == txId (inputWitnessTx witness)
    && txiOutput i == txOutputs (inputWitnessTx witness) !! refIdx (txiOutputRef i)
    -- ^ The output of the transaction is consistent
    && (txId <$> inputWitnessOfflineTxs updLeastRecentTxs)
    == (snd <$> updateTransactions (inputWitnessUpdate updLeastRecentTxs))
    -- ^ The offline transactions of the witness are consistent
    && merkleTreeRoot ((\(ix, tx) -> (ix, txId tx)) <$> inputWitnessOnlineTxs updLeastRecentTxs)
    == updateOnlineTxsRoot (inputWitnessUpdate (inputWitnessDataLeastRecent witness))
    -- ^ The online transactions of the witness are consistent

-- | Checks if the input was not spent.
inputNotSpent ::
       Signature context
    => Input context
    -- ^ The transaction input to check.
    -> InputWitness context
    -- ^ The witness data for the offline input.
    -> Bool context
inputNotSpent i witness =
    let inputs = concat (txInputs <$> inputWitnessAllTxs witness)
        expectedTxs = txId <$> inputWitnessAllTxs witness
        actualOnlineTxs = snd <$> inputWitnessDataRest witness
        actualOfflineTxs = updateTransactions . fst <$> inputWitnessDataRest witness
        actualTxs = concat $ actualOnlineTxs ++ actualOfflineTxs
        wUpdates = (fst <$> inputWitnessDataRest witness)
          ++ singleton (inputWitnessUpdate (inputWitnessDataLeastRecent witness))
        expectedMerkleRoots = updateOnlineTxsRoot <$> wUpdates
        actualMerkleRoots = merkleTreeRoot . snd <$> inputWitnessDataRest witness
    in all (/= i) inputs
    -- ^ check that inputs are unspent
    && expectedTxs == (snd <$> actualTxs)
    && expectedMerkleRoots == actualMerkleRoots
    -- ^ consistency checks

-- | Checks if the input is valid.
inputIsValid ::
       Signature context
    => Hash context
    -- ^ The id of the current block.
    -> Input context
    -- ^ The transaction input to check.
    -> InputWitness context
    -- ^ The witness data for the offline input.
    -> Bool context
inputIsValid bId i w =
       inputExisted bId i w
    && inputNotSpent i w
