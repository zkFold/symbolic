module ZkFold.Symbolic.Ledger.Types.Address (
  Address,
) where

import           Prelude                                       hiding (Bool, Eq, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Ledger.Types.Hash             (HashSimple)

-- | Address on the zkFold ledger.
--
-- Circuit describes the smart contract that locks funds at this address.
--
-- Data availability index points to a data availability source that posts data about the address's transactions. The data availability type indicates whether the address is associated with online or offline transactions.
type Address context = HashSimple context
