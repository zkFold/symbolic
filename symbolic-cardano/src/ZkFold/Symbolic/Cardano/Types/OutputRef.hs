{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
-- Avoid reduction overflow error caused by NumberOfRegisters
{-# OPTIONS_GHC -freduction-depth=0 #-}

module ZkFold.Symbolic.Cardano.Types.OutputRef where

import GHC.Generics (Generic, Generic1)
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq
import ZkFold.Symbolic.Data.UInt (KnownRegisters, RegisterSize (..))
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Class (Symbolic)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))
import qualified Prelude as Haskell

import ZkFold.Symbolic.Cardano.Types.Basic

type TxRefId context = ByteString 256 context

type TxRefIndex context = UInt 32 Auto context

data OutputRef context = OutputRef
  { outputRefId :: TxRefId context
  , outputRefIndex :: TxRefIndex context
  }
  deriving (Generic, Generic1, SymbolicData)

deriving instance Haskell.Eq context => Haskell.Eq (OutputRef context)

instance Symbolic c => Collect (ConstrainedDatum c) (OutputRef c)

instance
  (Symbolic context, KnownRegisters context 32 Auto)
  => Eq (OutputRef context)
