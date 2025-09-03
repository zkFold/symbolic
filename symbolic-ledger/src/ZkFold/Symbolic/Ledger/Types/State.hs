{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.State (
  State (..),
) where

import GHC.Generics (Generic)
import ZkFold.Data.Eq (Eq)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Ledger.Types.Address (Address)
import ZkFold.Symbolic.Ledger.Types.Hash (HashSimple)
import ZkFold.Symbolic.Ledger.Types.Nonce (KnownRegistersNonce, Nonce)
import ZkFold.Symbolic.Ledger.Types.Root (Root)
import ZkFold.Symbolic.Ledger.Types.Value (AssetValues, KnownRegistersAssetQuantity)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

-- | Defines the on-chain representation of the Symbolic Ledger state transition.
data State context = State
  { sPreviousBatch :: HashSimple context
  -- ^ Hash of the previous state.
  , sBridgeIn :: HashSimple context
  -- ^ Hash of the 'AssetValues' that are bridged into the ledger.
  , sBridgeOut :: HashSimple context
  -- ^ Hash of the 'AssetValues' that are bridged out of the ledger.
  , sDataHash :: HashSimple context
  -- ^ Hash of the transaction batch.
  , sAccountInfo :: Root (Address context, Nonce context, Root (AssetValues context))
  -- ^ Merkle tree root for account information.
  }
  deriving stock Generic

instance
  ( KnownRegistersAssetQuantity context
  , KnownRegistersNonce context
  , Symbolic context
  )
  => SymbolicData (State context)

instance
  ( KnownRegistersAssetQuantity context
  , KnownRegistersNonce context
  , Symbolic context
  )
  => Eq (State context)
