{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.State (
  State (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic, Generic1)
import GHC.TypeNats (KnownNat)
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.HFunctor.Classes (HShow)
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hashable (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Types.Hash (HashSimple, hashFn)
import ZkFold.Symbolic.Ledger.Types.Value (KnownRegistersAssetQuantity)

-- | Defines the on-chain representation of the Symbolic Ledger state.
data State ud a context = State
  { sPreviousStateHash :: HashSimple context
  -- ^ Hash of the previous state.
  , sUTxO :: FieldElement context
  -- ^ Root hash of the UTxO Merkle tree.
  , sLength :: FieldElement context
  -- ^ Denotes length of the state chain.
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

instance
  forall ud a context
   . ( KnownRegistersAssetQuantity context
     , Symbolic context
     )
  => Eq (State ud a context)

deriving stock instance (HShow context, Show (WitnessField context)) => Haskell.Show (State ud a context)

instance Symbolic context => Hashable (HashSimple context) (State ud a context) where
  hasher = hashFn

deriving anyclass instance
  forall ud a. ToJSON (State ud a RollupBFInterpreter)

deriving anyclass instance
  forall ud a. FromJSON (State ud a RollupBFInterpreter)

deriving anyclass instance
  forall ud a
   . (KnownNat ud, KnownNat a)
  => ToSchema (State ud a RollupBFInterpreter)
