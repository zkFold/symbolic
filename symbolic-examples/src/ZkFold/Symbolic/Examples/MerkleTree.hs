{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples.MerkleTree (exampleMerkleTreeInsert) where

import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.MerkleTree (KnownMerkleTree, MerkleEntry, MerkleTree, replace)

-- | Example of a Merkle tree insertion (replace) operation.
-- This is used for benchmarking the circuit size of Merkle tree insertions.
-- The depth parameter @d@ determines the size of the Merkle tree (2^(d-1) leaves).
exampleMerkleTreeInsert
  :: forall d c
   . (Symbolic c, KnownMerkleTree d)
  => MerkleEntry d c
  -> MerkleTree d c
  -> MerkleTree d c
exampleMerkleTreeInsert = replace
