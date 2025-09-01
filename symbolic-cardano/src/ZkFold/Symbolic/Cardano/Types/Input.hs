{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

-- Avoid reduction overflow error caused by NumberOfRegisters
{-# OPTIONS_GHC -freduction-depth=0 #-}

module ZkFold.Symbolic.Cardano.Types.Input where

import GHC.Generics (Generic, Generic1)
import ZkFold.Algebra.Number
import ZkFold.Data.Eq
import ZkFold.Data.HFunctor.Classes (HEq)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators (KnownRegisters, RegisterSize (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))
import qualified Prelude as Haskell

import ZkFold.Symbolic.Cardano.Types.Address (Address)
import ZkFold.Symbolic.Cardano.Types.Output (DatumHash, Output, txoAddress, txoDatumHash, txoTokens)
import ZkFold.Symbolic.Cardano.Types.OutputRef (OutputRef)
import ZkFold.Symbolic.Cardano.Types.Value (Value)

data Input tokens datum context = Input
  { txiOutputRef :: OutputRef context
  , txiOutput :: Output tokens datum context
  }
  deriving (Generic, Generic1, SymbolicData, SymbolicInput)

instance
  ( Symbolic context
  , KnownNat tokens
  , KnownRegisters context 32 Auto
  , KnownRegisters context 64 Auto
  )
  => Eq (Input tokens datum context)

deriving instance HEq context => Haskell.Eq (Input tokens datum context)

txiAddress :: Input tokens datum context -> Address context
txiAddress (Input _ txo) = txoAddress txo

txiTokens :: Input tokens datum context -> Value tokens context
txiTokens (Input _ txo) = txoTokens txo

txiDatumHash :: Input tokens datum context -> DatumHash context
txiDatumHash (Input _ txo) = txoDatumHash txo
