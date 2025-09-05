module ZkFold.Symbolic.Ledger.Types.Address (
  Address,
) where

import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Ledger.Types.Hash (Hash)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

-- | Address on the zkFold ledger.
type Address context = Hash (FieldElement context)