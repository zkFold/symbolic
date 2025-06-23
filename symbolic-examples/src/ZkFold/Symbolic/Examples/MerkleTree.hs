{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples.MerkleTree (exampleMerkleTree) where

import GHC.TypeNats
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (true))
import ZkFold.Symbolic.Data.Eq (Eq (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Maybe (fromJust)
import ZkFold.Symbolic.Data.MerkleTree
import ZkFold.Symbolic.Data.Morph
import ZkFold.Symbolic.Fold (SymbolicFold)

exampleMerkleTree
  :: forall d c
   . (SymbolicFold c, KnownNat d, 1 <= d)
  => MerkleTree d (FieldElement c) -> FieldElement c -> Bool c
exampleMerkleTree t x =
  x == lookup t (fromJust (findPath (Morph \(_ :: FieldElement s) -> (true :: Bool s)) t))
