module ZkFold.Symbolic.Ledger.Types.Datum (
  Datum,
) where

import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import Prelude hiding (Bool, Eq, Ord, length, splitAt, (*), (+))

-- | Datum that is attached to a transaction output.
type Datum context = FieldElement context
