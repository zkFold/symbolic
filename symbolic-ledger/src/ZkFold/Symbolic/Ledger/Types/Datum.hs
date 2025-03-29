module ZkFold.Symbolic.Ledger.Types.Datum (
  Datum,
) where

import           Prelude                           hiding (Bool, Eq, Ord, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Data.FieldElement (FieldElement)

-- | Datum that is attached to a transaction output.
type Datum context = FieldElement context
