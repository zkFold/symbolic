{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.State (
  State (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic, Generic1, (:.:))
import GHC.TypeNats (KnownNat)
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hashable (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.MerkleTree (KnownMerkleTree, MerkleTree)
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

import ZkFold.Symbolic.Ledger.Types.Field (RollupBF)
import ZkFold.Symbolic.Ledger.Types.Hash (Hash, HashSimple, hashFn)
import ZkFold.Symbolic.Ledger.Types.Transaction

-- | Defines the on-chain representation of the Symbolic Ledger state transition.
data State bi bo ud a context = State
  { sPreviousStateHash :: HashSimple context
  -- ^ Hash of the previous state.
  , sUTxO :: MerkleTree ud context
  -- ^ Merkle tree of UTxO set.
  , sLength :: FieldElement context
  -- ^ Denotes length of the state chain.
  , sBridgeIn :: Hash (Vector bi :.: Output a) context
  -- ^ Outputs that are bridged into the ledger. These lead to creation of new UTxOs where `orTxId` of the output is obtained by hashing `sLength` and `orIndex` is the index of the output in the vector.
  , sBridgeOut :: Hash (Vector bo :.: Output a) context
  -- ^ Denotes outputs that are bridged out of the ledger.
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (Eq, SymbolicData, SymbolicInput)

instance Symbolic c => Collect (ConstrainedDatum c) (State bi bo ud a c)

instance Symbolic context => Hashable (HashSimple context) (State bi bo ud a context) where
  hasher = hashFn

deriving anyclass instance
  forall bi bo ud a. KnownMerkleTree ud => ToJSON (State bi bo ud a RollupBF)

deriving anyclass instance
  forall bi bo ud a. KnownMerkleTree ud => FromJSON (State bi bo ud a RollupBF)

deriving anyclass instance
  forall bi bo ud a
   . (KnownNat bi, KnownNat bo, KnownNat ud, KnownNat a)
  => ToSchema (State bi bo ud a RollupBF)
