{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Symbolic.Cardano.Types.Address where

import GHC.Generics (Generic, Generic1)
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.HFunctor.Classes (HEq)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Input
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))
import qualified Prelude as Haskell

import ZkFold.Symbolic.Cardano.Types.Basic

type AddressType context = ByteString 4 context

type PaymentCredential context = ByteString 224 context

type StakingCredential context = ByteString 224 context

data Address context = Address
  { addressType :: AddressType context
  , paymentCredential :: PaymentCredential context
  , stakingCredential :: StakingCredential context
  }
  deriving (Generic, Generic1, SymbolicData, SymbolicInput)

deriving instance HEq context => Haskell.Eq (Address context)

instance Symbolic context => Eq (Address context)
