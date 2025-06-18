{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Hash where

import           Control.Monad                (return)
import           Data.Function                (($))
import           GHC.Generics                 (Generic, Generic1, Par1 (..))

import           ZkFold.Algebra.Class
import           ZkFold.Symbolic.Class        (Symbolic (..), embedW, fromCircuit2F)
import           ZkFold.Symbolic.Data.Class   (SymbolicData (..), SymbolicDataConstraint)
import           ZkFold.Symbolic.Data.Eq      (Eq (..), (==))
import           ZkFold.Symbolic.Data.Input   (SymbolicInput)
import           ZkFold.Symbolic.Data.Witness (Wit (..))
import           ZkFold.Symbolic.MonadCircuit (constraint)

-- | A generic hashing interface for Symbolic DSL.
-- 'h' is the result of the hashing algorithm;
-- 'a' is the datatype being hashed.
--
-- The relationship between datatypes and hashes is many-to-many
-- so there's no functional dependency in either direction.
class Hashable h x where
  -- | Hashing algorithm itself.
  hasher :: x -> h

-- | An invertible hash 'h' of a symbolic datatype 'a'.
data Hash h x c = Hash
  { hHash  :: h c
  , hValue :: Wit x c
  }
  deriving (Generic, Generic1)

instance (SymbolicData h, SymbolicDataConstraint x) => SymbolicData (Hash h x)
instance (SymbolicInput h, SymbolicDataConstraint x) => SymbolicInput (Hash h x)

-- | Restorably hash the data.
hash :: forall h x c . (Symbolic c, Hashable (h c) (x c), SymbolicDataConstraint x)
  => x c -> Hash h x c
hash a = Hash (hasher a) $ fromContext @(Wit x) (toContext a)

-- | Restore the data which were hashed.
preimage :: forall h x c . (Symbolic c, Hashable (h c) (x c), SymbolicDataConstraint x, SymbolicDataConstraint h)
  => Hash h x c -> x c
preimage Hash {..} =
  let Wit w = hValue
      raw = fromContext $ embedW w :: x c
      b = hasher raw == hHash
   in fromContext $
      ( fromCircuit2F (toContext raw) (toContext b) $ \r (Par1 i) -> do
          constraint (($ i) - one)
          return r
      )
