{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators  #-}


module Examples.MerkleTree (exampleMerkleTree) where


import           Data.Semialign                   (Zip)
import           Data.Type.Equality               (type (~))
import           GHC.TypeNats

import           ZkFold.Base.Algebra.Basic.Class  (NumberOfBits)
import           ZkFold.Symbolic.Class            (Symbolic (..))
import           ZkFold.Symbolic.Data.Bool        (Bool, BoolType (true))
import           ZkFold.Symbolic.Data.Class       (Context, Layout, SymbolicOutput)
import           ZkFold.Symbolic.Data.Combinators (KnownRegisters, NumberOfRegisters, RegisterSize (Auto))
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
