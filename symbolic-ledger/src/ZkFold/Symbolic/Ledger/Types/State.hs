{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.State (
  AccountInfo,
  State (..),
) where

import GHC.Generics (Generic, Generic1, (:.:), type (:*:) (..))
import GHC.TypeNats (KnownNat)
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

import ZkFold.Symbolic.Ledger.Types.Address (Address)
import ZkFold.Symbolic.Ledger.Types.Hash (Hash, HashSimple)
import ZkFold.Symbolic.Ledger.Types.Nonce (Nonce)
import ZkFold.Symbolic.Ledger.Types.Value (AssetValue, KnownRegistersAssetQuantity)

-- | Account information for a given user. Contains user's address, nonce and root hash of their asset merle tree.
type AccountInfo users = Hash (Vector users :.: (Address :*: Nonce :*: HashSimple))

-- | Defines the on-chain representation of the Symbolic Ledger state transition.
data State bi bo users context = State
  { sPreviousStateHash :: HashSimple context
  -- ^ Hash of the previous state.
  , sBridgeIn :: Hash (Vector bi :.: (Address :*: AssetValue)) context
  -- ^ Assets that are bridged into the ledger.
  -- We don't make it a nested vector as that would impose a length of nested vector even if user is not bridging more than one asset.
  , sBridgeOut :: Hash (Vector bo :.: (Address :*: Address :*: AssetValue)) context
  -- ^ Assets that are bridged out of the ledger. In the format of (from, to, asset). Following the same reasoning as bridged in assets, we don't make it a nested vector.
  , sAccountInfo :: AccountInfo users context
  -- ^ Merkle tree root for account information.
  }
  deriving stock (Generic, Generic1)
  deriving anyclass SymbolicData

instance
  ( KnownRegistersAssetQuantity context
  , KnownNat bi
  , KnownNat bo
  , Symbolic context
  )
  => Eq (State bi bo users context)
