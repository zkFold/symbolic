{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.Transaction.BatchData (
  TransactionBatchData (..),
) where

import GHC.Generics (Generic, Generic1, (:*:))
import ZkFold.Data.Eq (Eq)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Combinators (KnownRegisters, RegisterSize (Auto))
import ZkFold.Symbolic.Data.List (List)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

import ZkFold.Symbolic.Ledger.Types.Address (Address)
import ZkFold.Symbolic.Ledger.Types.Hash (HashSimple)
import ZkFold.Symbolic.Ledger.Types.Root (Root)
import ZkFold.Symbolic.Ledger.Types.Transaction.Core (KnownRegistersOutputIndex)
import ZkFold.Symbolic.Ledger.Types.Value (KnownRegistersAssetQuantity)

-- | Transaction batch data, given to us by a data provider source. Thus all the 'Address' entries here must have same 'DAIndex'.
data TransactionBatchData context = TransactionBatchData
  { tbdMerkleRoot :: Root (Address :*: List HashSimple) context
  -- ^ Merkle tree root of the addresses corresponding to online transactions (represented by their transaction hash) that are associated with a particular data availability source.
  , tbdOnlineAddresses :: List Address context
  -- ^ List of addresses included in the batch which correspond to online transactions and are associated with a particular data availability source.
  , tbdOfflineTransactions :: List (Address :*: List HashSimple) context
  -- ^ All offline transactions (represented by their transaction hash) associated with a particular data availability source that are included in the batch.
  }
  deriving stock (Generic, Generic1)
  deriving anyclass SymbolicData

instance
  ( KnownRegistersAssetQuantity context
  , KnownRegistersOutputIndex context
  , KnownRegisters context 11 Auto
  , Symbolic context
  )
  => Eq (TransactionBatchData context)
