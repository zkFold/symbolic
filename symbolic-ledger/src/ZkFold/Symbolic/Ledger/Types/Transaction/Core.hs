{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.Transaction.Core (
  OutputRef (..),
  Output (..),
  UTxO (..),
  Transaction (..),
  TransactionId,
  txId,
) where

import GHC.Generics (Generic, Generic1, (:*:), (:.:))
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto))
import ZkFold.Symbolic.Data.Hash (Hashable, hash)
import ZkFold.Symbolic.Data.UInt (UInt)
import ZkFold.Symbolic.Ledger.Types.Address (Address)
import ZkFold.Symbolic.Ledger.Types.Hash (Hash, HashSimple)
import ZkFold.Symbolic.Ledger.Types.Value (AssetValue, KnownRegistersAssetQuantity)
import Prelude hiding (Bool, Eq, Maybe, length, splitAt, (*), (+), (==), (||))
import qualified Prelude as Haskell hiding ((||))

-- | An output's reference.
data OutputRef context = OutputRef
  { orTxId :: HashSimple context
  -- ^ Transaction ID which created this output.
  , orIndex :: UInt 32 Auto context
  -- ^ Index of the output in the transaction.
  -- TODO: Restrict to represent 'o' outputs instead of 2^32?
  }
  deriving stock (Generic, Generic1)
  deriving anyclass SymbolicData

instance
  Symbolic context
  => Eq (OutputRef context)

-- | An output of a transaction.
data Output a context = Output
  { oAddress :: Address context
  -- ^ Address of the output.
  , oAssets :: (Vector a :.: AssetValue) context
  -- ^ Assets of the output.
  }
  deriving stock (Generic, Generic1)
  deriving anyclass SymbolicData

instance
  ( Symbolic context
  , KnownRegistersAssetQuantity context
  )
  => Eq (Output a context)

data UTxO a context = UTxO
  { uRef :: OutputRef context
  , uOutput :: Output a context
  }
  deriving stock (Generic, Generic1)
  deriving anyclass SymbolicData

instance
  ( Symbolic context
  , KnownRegistersAssetQuantity context
  )
  => Eq (UTxO a context)

-- | Transaction in our symbolic ledger.
data Transaction i o a context = Transaction
  { inputs :: (Vector i :.: OutputRef) context
  -- ^ Inputs.
  , outputs :: (Vector o :.: (Output a :*: Bool)) context
  -- ^ Outputs. Boolean denotes whether the output is a bridge out output, in which case `oAddress` denotes Cardano address.
  }
  deriving stock (Generic, Generic1)
  deriving anyclass SymbolicData

instance
  forall i o a context
   . ( Symbolic context
     , KnownRegistersAssetQuantity context
     )
  => Eq (Transaction i o a context)

-- | Transaction hash.
type TransactionId i o a = Hash (Transaction i o a)

txId
  :: forall i o a context
   . ( Symbolic context
     , Hashable (HashSimple context) (Transaction i o a context)
     )
  => Transaction i o a context
  -> TransactionId i o a context
txId = hash
