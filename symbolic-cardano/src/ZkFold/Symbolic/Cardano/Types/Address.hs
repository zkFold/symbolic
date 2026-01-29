{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Cardano.Types.Address where

import GHC.Generics (Generic, Generic1)
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq (Eq)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

import ZkFold.Symbolic.Cardano.Types.Basic

type AddressType context = ByteString 4 context

type PaymentCredential context = ByteString 224 context

type StakingCredential context = ByteString 224 context

data Address context = Address
  { addressType :: AddressType context
  , paymentCredential :: PaymentCredential context
  , stakingCredential :: StakingCredential context
  }
  deriving (Eq, Generic, Generic1, SymbolicData)

instance Symbolic c => Collect (ConstrainedDatum c) (Address c)
