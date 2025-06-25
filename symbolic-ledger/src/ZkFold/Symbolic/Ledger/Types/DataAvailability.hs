module ZkFold.Symbolic.Ledger.Types.DataAvailability (
  DAIndex,
  DAType,
  isOffline,
  isOnline,
) where

import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.Eq
import ZkFold.Symbolic.Data.FieldElement (FieldElement)

-- | Index associated with a data availability source.
type DAIndex context = FieldElement context

-- | Denotes data availability type, 'True' corresponds to offline transactions, while 'False' denotes online transactions.
type DAType context = Bool context

-- | Does 'DAType' correspond to offline transaction?
isOffline :: forall context. Symbolic context => DAType context -> Bool context
isOffline t = t == (true :: Bool context)

-- | Does 'DAType' correspond to online transaction?
isOnline :: forall context. Symbolic context => DAType context -> Bool context
isOnline t = t == (false :: Bool context)
