module ZkFold.Symbolic.Ledger.Types.DataAvailability (
  DAIndex,
  DAType,
) where

import           Prelude                           hiding (Bool, Eq, Ord, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Data.Bool         (Bool)
import           ZkFold.Symbolic.Data.FieldElement (FieldElement)

-- | Index associated with a data availability source.
type DAIndex context = FieldElement context

-- | Denotes data availability type, 'True' corresponds to offline transactions, while 'False' denotes online transactions.
type DAType context = Bool context
