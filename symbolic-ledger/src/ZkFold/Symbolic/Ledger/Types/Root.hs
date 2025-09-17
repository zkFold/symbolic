module ZkFold.Symbolic.Ledger.Types.Root (
  Root,
  empty,
  insert,
) where

import ZkFold.Symbolic.Ledger.Types.Hash (Hash)
import Prelude hiding (Bool, Eq, Ord, length, splitAt, (*), (+))

-- TODO: Move from Hash to correct type from symbolic-base.

-- | Denotes root of a Merkle tree with leaves of type @t@.
type Root = Hash

empty :: Root t c
empty = undefined

insert :: t c -> Root t c -> Root t c
insert = undefined
