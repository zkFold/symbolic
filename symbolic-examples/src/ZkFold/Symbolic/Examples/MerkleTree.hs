{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples.MerkleTree (exampleMerkleTreeInsert) where

import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.MerkleTree (KnownMerkleTree, MerkleEntry, MerkleTree, replace)

-- | Example of a Merkle tree insertion (replace) operation.
-- This is used for benchmarking the circuit size of Merkle tree insertions.
-- The depth parameter @d@ determines the size of the Merkle tree (2^(d-1) leaves).
--
-- == Circuit Size Analysis (PlonkUp format)
--
-- For depth @d@, the tree has 2^(d-1) leaves and (d-1) levels.
--
-- === Theoretical Minimum (path-based update)
-- * Merkle proof verification: (d-1) MiMC hashes
-- * New root computation: (d-1) MiMC hashes
-- * MiMC hash: 220 rounds × 3 constraints = 660 constraints
-- * Total: 2 × (d-1) × 660 constraints
-- * For d=5: 2 × 4 × 660 = 5,280 constraints
--
-- === Current Implementation
-- The current 'replace' function uses 'mapWithIx' which iterates over all
-- leaves, causing O(n) overhead where n = 2^(d-1). This could be optimized
-- to use path-based updates that only touch the nodes on the path from leaf
-- to root, reducing complexity from O(n) to O(log n).
exampleMerkleTreeInsert
  :: forall d c
   . (Symbolic c, KnownMerkleTree d)
  => MerkleEntry d c
  -> MerkleTree d c
  -> MerkleTree d c
exampleMerkleTreeInsert = replace
