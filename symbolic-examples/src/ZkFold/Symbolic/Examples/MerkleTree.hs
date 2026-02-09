{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples.MerkleTree (exampleMerkleTreeReplace) where

import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.MerkleTree (KnownMerkleTree, MerkleEntry, MerkleTree, replace)

-- | Merkle tree replace operation for circuit size benchmarking.
exampleMerkleTreeReplace
  :: forall d c
   . (Symbolic c, KnownMerkleTree d)
  => MerkleEntry d c
  -> MerkleTree d c
  -> MerkleTree d c
exampleMerkleTreeReplace = replace
