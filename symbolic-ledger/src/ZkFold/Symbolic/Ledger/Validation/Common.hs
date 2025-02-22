module ZkFold.Symbolic.Ledger.Validation.Common where

import           Prelude                      hiding (Bool, Eq (..), all, length, splitAt, zip, (&&), (*), (+), (==))

import           ZkFold.Symbolic.Data.Bool    (Bool)
import           ZkFold.Symbolic.Ledger.Types

-- | References in the chain of updates are consistent.
updateChainIsValid ::
      UpdateChain context
   -> Bool context
updateChainIsValid _ = undefined
