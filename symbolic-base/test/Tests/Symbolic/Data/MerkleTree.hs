{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module Tests.Symbolic.Data.MerkleTree
  ( testId
  , testLookup
  ) where


import           Data.Semialign                   (Zip)
import           Data.Type.Equality               (type (~))
import           GHC.Generics                     (Par1 (Par1))
import           GHC.TypeNats
import           Prelude                          (Eq, Show, ($), (.))
import           Test.QuickCheck                  (Property, (===))

import           ZkFold.Base.Algebra.Basic.Class  (FromConstant (..))
import           ZkFold.Base.Data.Vector          (Vector (..), (!!))
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators (Iso (..), KnownRegisters, RegisterSize (..))
import           ZkFold.Symbolic.Data.MerkleTree
import           ZkFold.Symbolic.Fold

-- import ZkFold.Symbolic.Data.Bool (Bool)
-- import ZkFold.Symbolic.Data.Morph
-- import ZkFold.Symbolic.Data.Conditional (Conditional)
-- import ZkFold.Symbolic.Data.Input (SymbolicInput)


-- type AC a = ArithmeticCircuit a U1 U1


testId :: forall x c d n.
  ( SymbolicOutput x
  , Context x ~ c
  , KnownNat d
  , SymbolicFold c
  , Zip (Layout x)
  , n ~ 2 ^ d
  , Show x, Eq x
  ) => Vector n x -> Property
testId v = v === (from (from v :: MerkleTree d x) :: Vector n x)

testLookup :: forall x c d n.
  ( SymbolicOutput x
  , Context x ~ c
  , KnownNat (d - 1)
  , KnownNat d
  , SymbolicFold c
  , KnownRegisters c d 'Auto
  , Zip (Layout x)
  , n ~ 2 ^ d
  , Show x, Eq x
  ) => Vector n x -> Natural -> Property
testLookup v n = (v !! n) === lookup (from v :: MerkleTree d x) (MerkleTreePath . indToPath @c @d . embed . Par1 $ fromConstant n)

-- testFind :: forall c x d n.
--   ( SymbolicInput x
--   , Context x ~ c, KnownNat d
--   , SymbolicFold c
--   , n ~ 2 ^ d
--   , Eq x, Conditional (Bool c) x, Eq (c Par1), Show (M.Maybe c x)
--   ) => (x -> Bool c) -> Vector n x -> Property
-- testFind pred v = M.find pred (toV v) === find (Morph pred :: MorphFrom c x (Bool c)) (from v :: MerkleTree d x)
