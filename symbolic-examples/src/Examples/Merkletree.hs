{-# LANGUAGE TypeOperators        #-}

module Examples.MerkleTree (exampleMerkleTree) where


import ZkFold.Symbolic.Data.MerkleTree
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.Class (SymbolicOutput, Context)
import ZkFold.Symbolic.Data.Combinators (KnownRegisters, RegisterSize (Auto))
import ZkFold.Symbolic.Fold (SymbolicFold)
import           GHC.TypeNats
import           Data.Type.Equality                             (type (~))
import ZkFold.Symbolic.Data.Eq (Eq(..))
import ZkFold.Symbolic.Data.Maybe (fromJust)


exampleMerkleTree ::
  ( SymbolicOutput x
  , Context x ~ c
  , KnownNat (d - 1)
  , KnownNat d
  , SymbolicFold c
  , KnownRegisters c d 'Auto
  , BooleanOf x ~ Bool c
  ) => MerkleTree d x -> x -> Bool c
exampleMerkleTree t x = x == lookup t (fromJust (findPath (Morh (x ==)) t))