{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.Transaction.Core (
  Transaction (..),
  TransactionId,
  txId,
) where

import GHC.Generics (Generic, Generic1, type (:*:) (..))
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Hash (Hashable, hash)
import ZkFold.Symbolic.Ledger.Types.Address (Address)
import ZkFold.Symbolic.Ledger.Types.Hash (Hash, HashSimple)
import ZkFold.Symbolic.Ledger.Types.Nonce (Nonce)
import ZkFold.Symbolic.Ledger.Types.Value (AssetValue, KnownRegistersAssetQuantity)
import Prelude hiding (Bool, Eq, Maybe, length, splitAt, (*), (+), (==), (||))
import qualified Prelude as Haskell hiding ((||))

-- | Transaction in our symbolic ledger.
data Transaction context = Transaction
  { from :: Address context
  -- ^ Address of the sender.
  , to :: Address context
  -- ^ Address of the receiver.
  , nonce :: Nonce context
  -- ^ Number of transactions sent by the sender.
  , asset :: AssetValue context
  -- ^ Asset being transferred.
  , isBridgeOut :: Bool context
  }
  deriving stock (Generic, Generic1)
  deriving anyclass SymbolicData

instance
  ( KnownRegistersAssetQuantity context
  , Symbolic context
  )
  => Eq (Transaction context)

-- | Transaction hash.
type TransactionId = Hash Transaction

txId
  :: ( KnownRegistersAssetQuantity context
     , Symbolic context
     , Hashable (HashSimple context) (Transaction context)
     )
  => Transaction context
  -> TransactionId context
txId = hash
