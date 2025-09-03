module ZkFold.Symbolic.Ledger.Types.Root (
  Root,
  empty,
  insert,
) where

import Prelude hiding (Bool, Eq, Ord, length, splitAt, (*), (+))

import ZkFold.Symbolic.Ledger.Types.Hash (Hash)

-- TODO: Move from Hash to correct type from symbolic-base.

-- | Denotes root of a Merkle tree with leaves of type @t@.
type Root t = Hash t

empty :: Root t
empty = undefined

insert :: t -> Root t -> Root t
insert = undefined
