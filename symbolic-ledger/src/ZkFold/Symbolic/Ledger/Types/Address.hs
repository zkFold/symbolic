module ZkFold.Symbolic.Ledger.Types.Address (
  Address,
  nullAddress,
) where

import ZkFold.Algebra.Class (Zero (..))
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

-- | Address on the zkFold ledger.
type Address = FieldElement

nullAddress :: Symbolic context => Address context
nullAddress = zero
