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

type Interval context = (UTCTime context, UTCTime context)

-- | @a contains b@ is true if @b@ is entirely contained in @a@.
contains
  :: (KnownNat (Ceil regSize OrdWord), KnownRegisters c 11 Auto, Symbolic c, regSize ~ GetRegisterSize (BaseField c) 11 Auto)
  => Interval c -> Interval c -> Bool c
contains (as, ae) (bs, be) = as <= bs && be <= ae
