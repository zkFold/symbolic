{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.Transaction.Core (
  Transaction (..),
  TransactionId,
  txId,
) where

import GHC.Generics (Generic)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Combinators (KnownRegisters, RegisterSize (Auto))
import ZkFold.Symbolic.Data.Hash (Hashable, hash)
import Prelude hiding (Bool, Eq, Maybe, length, splitAt, (*), (+), (==), (||))
import qualified Prelude as Haskell hiding ((||))

import ZkFold.Symbolic.Ledger.Types.Address (Address)
import ZkFold.Symbolic.Ledger.Types.Hash (Hash, HashSimple)
import ZkFold.Symbolic.Ledger.Types.Nonce (Nonce)
import ZkFold.Symbolic.Ledger.Types.Value (AssetValue, KnownRegistersAssetQuantity)

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
  }
  deriving stock Generic

instance
  ( KnownRegistersAssetQuantity context
  , Symbolic context
  )
  => SymbolicData (Transaction context)

instance
  ( KnownRegistersAssetQuantity context
  , KnownRegisters context 256 Auto
  , Symbolic context
  )
  => Eq (Transaction context)

-- | Transaction hash.
type TransactionId context = Hash (Transaction context)

txId
  :: ( KnownRegistersAssetQuantity context
     , Symbolic context
     , Hashable (HashSimple context) (Transaction context)
     )
  => Transaction context
  -> TransactionId context
txId = hash
