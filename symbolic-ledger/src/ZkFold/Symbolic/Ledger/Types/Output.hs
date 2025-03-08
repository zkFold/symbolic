module ZkFold.Symbolic.Ledger.Types.Output where

import           Prelude                              hiding (Bool, Eq, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Ledger.Types.Address (Address, Datum)
import           ZkFold.Symbolic.Ledger.Types.Value   (Value)

-- | Transaction output.
data Output context = Output
        { txoAddress :: Address context
        -- ^ Address at which the value is locked
        , txoValue   :: Value context
        -- ^ Value locked by the output
        , txoDatum   :: Datum context
        -- ^ Datum associated with the output
        }
