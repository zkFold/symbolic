{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators  #-}


module Examples.MerkleTree (exampleMerkleTree) where


import           Data.Type.Equality               (type (~))
import           GHC.TypeNats

import           ZkFold.Symbolic.Data.Bool        (Bool, BoolType (true))
import           ZkFold.Symbolic.Data.Class       (Context, SymbolicOutput, Layout)
import           ZkFold.Symbolic.Data.Combinators (KnownRegisters, RegisterSize (Auto), NumberOfRegisters)
import           ZkFold.Symbolic.Data.Eq          (Eq (..))
import           ZkFold.Symbolic.Data.Maybe       (fromJust)
import           ZkFold.Symbolic.Data.MerkleTree
import           ZkFold.Symbolic.Fold             (SymbolicFold)
import           ZkFold.Symbolic.Data.Morph
import Data.Semialign (Zip)
import ZkFold.Symbolic.Class (Symbolic(..))
import ZkFold.Symbolic.Data.Switch (Switch)
import ZkFold.Base.Algebra.Basic.Class (NumberOfBits)


exampleMerkleTree :: forall c x d n .
  ( SymbolicOutput x
  , Context x ~ c
  , SymbolicFold c
  , KnownNat (d - 1)
  , KnownNat d
  , BooleanOf x ~ Bool c
  , KnownNat (NumberOfRegisters (BaseField c) n Auto)
  , NumberOfBits (BaseField c) ~ n
  , KnownRegisters c d Auto
  , Zip (Layout x)
  , Eq x
  ) => MerkleTree d x -> x -> Bool c
exampleMerkleTree t x = x == lookup t (fromJust (findPath (Morph \ (_ :: Switch s x) -> (true :: Bool s)) t))
