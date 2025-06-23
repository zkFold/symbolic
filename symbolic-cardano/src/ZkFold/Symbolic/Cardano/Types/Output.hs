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

import GHC.Generics (Generic)
import ZkFold.Algebra.Number
import ZkFold.Data.HFunctor.Classes (HEq)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators (KnownRegisters, RegisterSize (..))
import ZkFold.Symbolic.Data.Conditional (Conditional)
import ZkFold.Symbolic.Data.Eq (Eq)
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))
import qualified Prelude as Haskell

import ZkFold.Symbolic.Cardano.Types.Address (Address)
import ZkFold.Symbolic.Cardano.Types.Basic
import ZkFold.Symbolic.Cardano.Types.Output.Datum
import ZkFold.Symbolic.Cardano.Types.Value (SingleAsset, Value)

data Liability context
  = Liability
  { lLiability :: SingleAsset context -- Liability in native tokens
  , lBabel :: SingleAsset context -- Offer in any other tokens
  }

deriving instance Generic (Liability context)

deriving instance HEq context => Haskell.Eq (Liability context)

deriving instance (KnownRegisters context 64 Auto, Symbolic context) => SymbolicData (Liability context)

-- TODO: derive this automatically
instance
  ( KnownRegisters context 64 Auto
  , Symbolic context
  )
  => SymbolicInput (Liability context)
  where
  isValid Liability {..} = isValid (lLiability, lBabel)

data Output tokens datum context = Output
  { txoAddress :: Address context
  , txoTokens :: Value tokens context
  , txoDatumHash :: DatumHash context
  }

deriving instance Generic (Output tokens datum context)

deriving instance HEq context => Haskell.Eq (Output tokens datum context)

deriving instance
  (KnownNat tokens, KnownRegisters context 64 Auto, Symbolic context) => SymbolicData (Output tokens datum context)

instance
  ( KnownNat tokens
  , KnownRegisters context 64 Auto
  , Symbolic context
  )
  => SymbolicInput (Output tokens datum context)
  where
  isValid (Output a t d) = isValid (a, t, d)

instance
  ( KnownNat tokens
  , KnownRegisters context 64 Auto
  , Symbolic context
  )
  => Conditional (Bool context) (Output tokens datum context)

instance
  ( KnownNat tokens
  , KnownRegisters context 64 Auto
  , Symbolic context
  )
  => Eq (Output tokens datum context)
