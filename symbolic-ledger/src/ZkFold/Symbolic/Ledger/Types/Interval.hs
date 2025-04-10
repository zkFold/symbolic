module ZkFold.Symbolic.Ledger.Types.Interval (
  Interval,
  contains,
) where

import           Prelude                          (type (~))

import           ZkFold.Base.Algebra.Basic.Number (KnownNat)
import           ZkFold.Symbolic.Class            (Symbolic (..))
import           ZkFold.Symbolic.Data.Bool        (Bool, BoolType (..))
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Ord
import           ZkFold.Symbolic.Data.UInt        (OrdWord)
import           ZkFold.Symbolic.Data.UTCTime     (UTCTime)

type Interval context = (UTCTime context, UTCTime context)

-- | @a contains b@ is true if @b@ is entirely contained in @a@.
contains :: (Symbolic c, KnownRegisters c 11 Auto, regSize ~ GetRegisterSize (BaseField c) 11 Auto, KnownNat (Ceil regSize OrdWord)) => Interval c -> Interval c -> Bool c
contains (as, ae) (bs, be) = as <= bs && be <= ae
