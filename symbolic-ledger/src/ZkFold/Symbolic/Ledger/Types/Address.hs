module ZkFold.Symbolic.Ledger.Types.Address (
  Address,
  nullAddress,
) where

import ZkFold.Algebra.Class (Zero (..))
import ZkFold.Symbolic.Compat (CompatData)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.V2 (Symbolic)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

-- | Address on the zkFold ledger.
type Address = CompatData FieldElement

-- | Null address.
nullAddress :: Symbolic context => Address context
nullAddress = zero
