module ZkFold.Symbolic.Ledger.Types.Address where

import           Prelude                                       hiding (Bool, Eq, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Ledger.Types.Circuit          (Circuit)
import           ZkFold.Symbolic.Ledger.Types.DataAvailability
import           ZkFold.Symbolic.Ledger.Types.Hash             (Hash)

-- | Address on the zkFold ledger.
--
-- Circuit describes the smart contract that locks funds at this address.
--
-- Data availability index points to a data availability source that posts data about the address's transactions. The data availability type indicates whether the address is associated with online or offline transactions.
type Address context = Hash (Circuit context, DAIndex context, DAType context)
