module ZkFold.Symbolic.Ledger.Validation.Transaction where

import           Prelude                                    hiding (Bool, Eq, all, length, splitAt, sum, (&&), (*), (+),
                                                             (++), (==))

import           ZkFold.Symbolic.Data.Bool                  (Bool, BoolType (..), all)
import           ZkFold.Symbolic.Data.Eq                    (Eq (..))
import           ZkFold.Symbolic.Data.List                  (List)
import           ZkFold.Symbolic.Ledger.Types
import           ZkFold.Symbolic.Ledger.Validation.Contract
import           ZkFold.Symbolic.Ledger.Validation.Input

-- | Witness data for the inputs of a transaction.
type TransactionInputsWitness context =
      List context (Input context, InputWitness context)

-- | Witness data for a transaction satisfies the included contracts.
type TransactionContractsWitness context =
      ( List context ((ContractId context, Datum context), SpendingContractWitness context)
      , List context ((ContractId context, Token context), MintingContractWitness context)
      )

-- | Witness data for a transaction.
type TransactionWitness context = (TransactionInputsWitness context, TransactionContractsWitness context)

-- | Checks if the inputs of a transaction are valid.
transactionInputsAreValid ::
      Signature context
   => Hash context
   -- ^ The id of the current block.
   -> Transaction context
   -- ^ The transaction to check.
   -> TransactionInputsWitness context
   -- ^ The witness data for the inputs of the transaction.
   -> Bool context
transactionInputsAreValid bId tx w =
   let inputs = txInputs tx

   in all (uncurry $ inputIsValid bId) w
   && inputs == fmap fst w

-- | Checks if a transaction satisfies the included contracts.
transactionContractsAreSatisfied ::
      Signature context
   => Transaction context
   -> TransactionContractsWitness context
   -> Bool context
transactionContractsAreSatisfied tx (wSpend, wMint) =
   let spendingContracts = (\(Output addr _ datum) -> (addr, datum)) . txiOutput <$> txInputs tx
       mintingContracts = (\(Value cId token _) -> (cId, token)) <$> txMint tx

   in all (\((addr, datum), w) -> spendingContractIsSatisfied tx addr datum w) wSpend
   && spendingContracts == fmap fst wSpend
   && all (\((cId, token), w) -> mintingContractIsSatisfied tx cId token w) wMint
   && mintingContracts == fmap fst wMint

-- | Checks if a transaction is valid.
transactionIsValid ::
      Signature context
   => Hash context
   -- ^ The id of the current block.
   -> Transaction context
   -- ^ The transaction to check.
   -> TransactionWitness context
   -- ^ The witness data for the inputs of the transaction.
   -> Bool context
transactionIsValid bId tx (wInputs, wContracts) =
       transactionInputsAreValid bId tx wInputs
    && transactionContractsAreSatisfied tx wContracts
