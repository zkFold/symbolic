{-# LANGUAGE DeriveAnyClass #-}
module ZkFold.Symbolic.Ledger.Types.Interval (
  Interval,
  contains,
) where

import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.Ord
import ZkFold.Symbolic.Data.UInt (OrdWord)
import ZkFold.Symbolic.Data.UTCTime (UTCTime)
import Prelude (type (~))
import GHC.Generics (Generic1, Generic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Data.Eq (Eq)

data Interval context = Interval
  { start :: UTCTime context
  , end :: UTCTime context
  }
  deriving (Generic, Generic1, SymbolicData)

instance Symbolic c => Eq (Interval c)

-- | @a contains b@ is true if @b@ is entirely contained in @a@.
contains
  :: (Symbolic c, KnownRegisters c 11 Auto, regSize ~ GetRegisterSize (BaseField c) 11 Auto, KnownNat (Ceil regSize OrdWord))
  => Interval c -> Interval c -> Bool c
contains (Interval as ae) (Interval bs be) = as <= bs && be <= ae
