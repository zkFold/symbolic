{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.Transaction.Batch (
  TransactionBatch (..),
) where

import GHC.Generics (Generic)
import ZkFold.Data.Eq (Eq)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.List (List)
import ZkFold.Symbolic.Ledger.Types.Address (Address)
import ZkFold.Symbolic.Ledger.Types.Hash (HashSimple)
import ZkFold.Symbolic.Ledger.Types.Nonce (KnownRegistersNonce)
import ZkFold.Symbolic.Ledger.Types.Root (Root)
import ZkFold.Symbolic.Ledger.Types.Value (KnownRegistersAssetQuantity)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

data TransactionBatch c = TransactionBatch
  { tbAddressTransactionsRoot :: Root (Address c, List c (HashSimple c))
  -- ^ Merkle tree root of the address's transactions.
  , tbAddresses :: List c (Address c)
  -- ^ List of addresses included in the batch which are sending funds.
  }
  deriving stock Generic

instance
  ( KnownRegistersAssetQuantity context
  , KnownRegistersNonce context
  , Symbolic context
  )
  => SymbolicData (TransactionBatch context)

instance
  ( KnownRegistersAssetQuantity context
  , KnownRegistersNonce context
  , Symbolic context
  )
  => Eq (TransactionBatch context)