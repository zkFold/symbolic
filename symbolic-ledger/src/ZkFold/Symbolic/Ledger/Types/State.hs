{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.State (
  State (..),
) where

import GHC.Generics (Generic, Generic1, (:.:))
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hashable (..))
import ZkFold.Symbolic.Data.MerkleTree (MerkleTree)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

import ZkFold.Symbolic.Ledger.Types.Hash (Hash, HashSimple, hashFn)
import ZkFold.Symbolic.Ledger.Types.Transaction
import ZkFold.Symbolic.Ledger.Types.Value (KnownRegistersAssetQuantity)
import ZkFold.Data.HFunctor.Classes (HShow)
import qualified Prelude as Haskell

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
  deriving anyclass SymbolicData

instance
  forall bi bo ud a context
   . ( KnownRegistersAssetQuantity context
     , Symbolic context
     )
  => Eq (State bi bo ud a context)

deriving stock instance (HShow context, Show (WitnessField context)) => Haskell.Show (State bi bo ud a context)

instance Symbolic context => Hashable (HashSimple context) (State bi bo ud a context) where
  hasher = hashFn
