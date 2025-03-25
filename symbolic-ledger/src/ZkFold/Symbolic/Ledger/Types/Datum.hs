module ZkFold.Symbolic.Ledger.Types.Datum (
  Datum,
) where

import           Prelude                           hiding (Bool, Eq, Ord, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Data.FieldElement (FieldElement)

-- | An output is tagged with a datum.
type Datum context = FieldElement context
