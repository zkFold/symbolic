{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

-- Avoid reduction overflow error caused by NumberOfRegisters
{-# OPTIONS_GHC -freduction-depth=0 #-}

module ZkFold.Symbolic.Cardano.Types.Output (
  module ZkFold.Symbolic.Cardano.Types.Output.Datum,
  Output (..),
  Liability (..),
) where

import GHC.Generics (Generic, Generic1)
import ZkFold.Algebra.Number
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.HFunctor.Classes (HEq)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators (KnownRegisters, RegisterSize (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))
import qualified Prelude as Haskell

import ZkFold.Symbolic.Cardano.Types.Address (Address)
import ZkFold.Symbolic.Cardano.Types.Output.Datum
import ZkFold.Symbolic.Cardano.Types.Value (SingleAsset, Value)

data Liability context = Liability
  { lLiability :: SingleAsset context -- Liability in native tokens
  , lBabel :: SingleAsset context -- Offer in any other tokens
  }
  deriving (Generic, Generic1, SymbolicData, SymbolicInput)

deriving instance HEq context => Haskell.Eq (Liability context)

data Output tokens datum context = Output
  { txoAddress :: Address context
  , txoTokens :: Value tokens context
  , txoDatumHash :: DatumHash context
  }
  deriving (Generic, Generic1, SymbolicData, SymbolicInput)

deriving instance HEq context => Haskell.Eq (Output tokens datum context)

instance
  ( Symbolic context
  , KnownNat tokens
  , KnownRegisters context 64 Auto
  )
  => Eq (Output tokens datum context)
