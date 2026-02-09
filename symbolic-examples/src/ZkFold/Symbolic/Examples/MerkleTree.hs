{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples.MerkleTree (exampleMerkleTreeInsert) where

import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.MerkleTree (KnownMerkleTree, MerkleEntry, MerkleTree, replace)

-- | Merkle tree insertion (replace) operation for circuit size benchmarking.
exampleMerkleTreeInsert
  :: forall d c
   . (Symbolic c, KnownMerkleTree d)
  => MerkleEntry d c
  -> MerkleTree d c
  -> MerkleTree d c
exampleMerkleTreeInsert = replace
