{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Cardano.Types.Output (
  module ZkFold.Symbolic.Cardano.Types.Output.Datum,
  Output (..),
  Liability (..),
) where

import GHC.Generics (Generic, Generic1)
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq (Eq)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

import ZkFold.Symbolic.Cardano.Types.Address (Address)
import ZkFold.Symbolic.Cardano.Types.Output.Datum
import ZkFold.Symbolic.Cardano.Types.Value (SingleAsset, Value)

data Liability context = Liability
  { lLiability :: SingleAsset context -- Liability in native tokens
  , lBabel :: SingleAsset context -- Offer in any other tokens
  }
  deriving (Generic, Generic1, SymbolicData)

instance Symbolic c => Collect (ConstrainedDatum c) (Liability c)

data Output tokens datum context = Output
  { txoAddress :: Address context
  , txoTokens :: Value tokens context
  , txoDatumHash :: DatumHash context
  }
  deriving (Eq, Generic, Generic1, SymbolicData)

instance Symbolic c => Collect (ConstrainedDatum c) (Output t d c)
