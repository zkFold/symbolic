module ZkFold.Symbolic.Ledger.Types.Hash where

import           Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

-- | Hash type used in the zkFold ledger.
data Hash context

class Hashable context x where
  hash :: x -> Hash context
