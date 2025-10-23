{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Cardano.Types.OutputRef where

import GHC.Generics (Generic, Generic1)
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

import ZkFold.Symbolic.Cardano.Types.Basic

type TxRefId context = ByteString 256 context

type TxRefIndex context = UInt 32 context

data OutputRef context = OutputRef
  { outputRefId :: TxRefId context
  , outputRefIndex :: TxRefIndex context
  }
  deriving (Eq, Generic, Generic1, SymbolicData)

instance Symbolic c => Collect (ConstrainedDatum c) (OutputRef c)
