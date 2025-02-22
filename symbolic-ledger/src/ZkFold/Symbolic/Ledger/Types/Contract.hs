module ZkFold.Symbolic.Ledger.Types.Contract where

import           Prelude                           hiding (Bool, Eq, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Data.Bool         (Bool)
import           ZkFold.Symbolic.Ledger.Types.Hash

-- | A contract is a specification on a transaction.
-- A contract must be valid if the corresponding triggering condition holds for the transaction.
-- See definitions of `SpendingContract` and `MintingContract` for details.
type Contract tx pub prv context = tx -> pub -> prv -> Bool context

type ContractId context = Hash context

contractId :: Contract tx pub prv context -> ContractId context
contractId _ = undefined

-- | Public data to be posted in the zkFold ledger update.
data ContractData context
