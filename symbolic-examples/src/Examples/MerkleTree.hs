{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators  #-}


module Examples.MerkleTree (exampleMerkleTree) where


import           Data.Type.Equality               (type (~))
import           GHC.TypeNats

import           ZkFold.Base.Algebra.Basic.Class  (NumberOfBits)
import           ZkFold.Symbolic.Class            (Symbolic (..))
import           ZkFold.Symbolic.Data.Bool        (Bool, BoolType (true))
import           ZkFold.Symbolic.Data.Class       (Context, SymbolicOutput)
import           ZkFold.Symbolic.Data.Eq          (Eq (..))
import           ZkFold.Symbolic.Data.Maybe       (fromJust)
import           ZkFold.Symbolic.Data.MerkleTree
import           ZkFold.Symbolic.Data.Morph
import           ZkFold.Symbolic.Data.Switch      (Switch)
import           ZkFold.Symbolic.Fold             (SymbolicFold)


exampleMerkleTree :: forall c x d n .
  ( SymbolicOutput x
  , Context x ~ c
  , SymbolicFold c
  , KnownNat d
  , BooleanOf x ~ Bool c
  , NumberOfBits (BaseField c) ~ n
  , Eq x
  , 1 <= d
  ) => MerkleTree d x -> x -> Bool c
exampleMerkleTree t x = x == lookup t (fromJust (findPath (Morph \ (_ :: Switch s x) -> (true :: Bool s)) t))
