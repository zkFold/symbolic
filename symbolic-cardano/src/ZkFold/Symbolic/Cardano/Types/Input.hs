{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Cardano.Types.Input where

import GHC.Generics (Generic, Generic1)
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

import ZkFold.Symbolic.Cardano.Types.Address (Address)
import ZkFold.Symbolic.Cardano.Types.Output
import ZkFold.Symbolic.Cardano.Types.OutputRef (OutputRef)
import ZkFold.Symbolic.Cardano.Types.Value (Value)

data Input tokens datum context = Input
  { txiOutputRef :: OutputRef context
  , txiOutput :: Output tokens datum context
  }
  deriving (Eq, Generic, Generic1, SymbolicData)

instance Symbolic c => Collect (ConstrainedDatum c) (Input t d c)

txiAddress :: Input tokens datum context -> Address context
txiAddress (Input _ txo) = txoAddress txo

txiTokens :: Input tokens datum context -> Value tokens context
txiTokens (Input _ txo) = txoTokens txo

txiDatumHash :: Input tokens datum context -> DatumHash context
txiDatumHash (Input _ txo) = txoDatumHash txo
