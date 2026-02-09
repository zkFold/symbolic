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
-- === Optimized Implementation (path-based update)
-- * Merkle path computation: (d-1) MiMC hashes (witness only, no constraints)
-- * New root computation via rootOnReplace: (d-1) MiMC hashes
-- * Verification via contains: (d-1) MiMC hashes
-- * Equality check (root comparison): 3 constraints
-- * Assert constraint: 1 constraint
-- * MiMC hash: 220 rounds × 3 constraints = 660 constraints
-- * Total: 2 × (d-1) × 660 + 4 constraints
-- * For d=5: 2 × 4 × 660 + 4 = 5,284 constraints
-- * For d=20: 2 × 19 × 660 + 4 = 25,084 constraints
--
-- Note: The current stats files may need to be regenerated after optimization.
-- The implementation uses O(log n) complexity by operating only on
-- the path from leaf to root, rather than iterating over all leaves.
--
-- === Lookup Table
-- There should be 0 lookup tables for pure MiMC-based Merkle operations,
-- as MiMC uses only polynomial constraints (no range checks needed).
exampleMerkleTreeInsert
  :: forall d c
   . (Symbolic c, KnownMerkleTree d)
  => MerkleEntry d c
  -> MerkleTree d c
  -> MerkleTree d c
exampleMerkleTreeInsert = replace
