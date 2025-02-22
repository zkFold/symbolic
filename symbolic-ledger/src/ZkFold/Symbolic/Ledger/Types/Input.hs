module ZkFold.Symbolic.Ledger.Types.Input where

import           Prelude                                hiding (Bool, Eq, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Ledger.Types.Output    (Output)
import           ZkFold.Symbolic.Ledger.Types.OutputRef (OutputRef)

-- | Input to a transaction.
data Input context = Input
    { txiOutputRef :: OutputRef context
    -- ^ Reference to the output being spent.
    , txiOutput    :: Output context
    -- ^ The output being spent.
    }
