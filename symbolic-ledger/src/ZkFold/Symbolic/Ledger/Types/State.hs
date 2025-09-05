{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.State (
  State (..),
) where

import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat)
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Ledger.Types.Address (Address)
import ZkFold.Symbolic.Ledger.Types.Hash (Hash, HashSimple)
import ZkFold.Symbolic.Ledger.Types.Nonce (Nonce)
import ZkFold.Symbolic.Ledger.Types.Root (Root)
import ZkFold.Symbolic.Ledger.Types.Value (AssetValue, AssetValues, KnownRegistersAssetQuantity)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

-- | Defines the on-chain representation of the Symbolic Ledger state transition.
data State context bi bo = State
  { sPreviousStateHash :: HashSimple context
  -- ^ Hash of the previous state.
  , sBridgeIn :: Hash (Vector bi (Address context, AssetValue context))
  -- ^ Assets that are bridged into the ledger.
  -- We don't make it a nested vector as that would impose a length of nested vector even if user is not bridging more than one asset.
  , sBridgeOut :: Hash (Vector bo (Address context, AssetValue context))
  -- ^ Assets that are bridged out of the ledger.
  -- We don't make it a nested vector as that would impose a length of nested vector even if user is not bridging more than one asset.
  , sAccountInfo :: Root (Address context, Nonce context, Root (AssetValues context))
  -- ^ Merkle tree root for account information.
  }
  deriving stock Generic

instance
  ( KnownRegistersAssetQuantity context
  , Symbolic context
  , KnownNat bi
  , KnownNat bo
  )
  => SymbolicData (State context bi bo)

instance
  ( KnownRegistersAssetQuantity context
  , KnownNat bi
  , KnownNat bo
  , Symbolic context
  )
  => Eq (State context bi bo)
