{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Hash where

import           Control.Monad                  (return)
import           Data.Function                  (($))
import           GHC.Generics                   (Generic, Generic1, Par1 (..))

import           ZkFold.Algebra.Class
import           ZkFold.Symbolic.Class          (Symbolic (witnessF), embedW, fromCircuit2F)
import           ZkFold.Symbolic.Data.Class     (SymbolicData (..), Wit (..))
import           ZkFold.Symbolic.Data.Eq        (Eq (..), (==))
import           ZkFold.Symbolic.Data.Input     (SymbolicInput)
import           ZkFold.Symbolic.Data.Payloaded (Payloaded (Payloaded))
import           ZkFold.Symbolic.MonadCircuit   (constraint)

-- | A generic hashing interface for Symbolic DSL.
-- 'h' is the result of the hashing algorithm;
-- 'a' is the datatype being hashed.
--
-- The relationship between datatypes and hashes is many-to-many
-- so there's no functional dependency in either direction.
class Hashable h a where
  -- | Hashing algorithm itself.
  hasher :: a -> h

-- | An invertible hash 'h' of a symbolic datatype 'a'.
data Hash h a c = Hash
  { hHash  :: h c
  , hValue :: Payloaded (Layout a) c
  }
  deriving (Generic, Generic1)

instance (SymbolicData h, SymbolicData a) => SymbolicData (Hash h a)
instance (SymbolicInput h, SymbolicInput a) => SymbolicInput (Hash h a)

-- | Restorably hash the data.
hash :: (Symbolic c, Hashable (h c) (a c), SymbolicData a) => a c -> Hash h a c
hash a = Hash (hasher a) $ Payloaded $ Wit $ witnessF (toContext a)

-- | Restore the data which were hashed.
preimage ::
  forall h a c .
  ( Symbolic c, Hashable (h c) (a c), SymbolicData a, SymbolicData h) => Hash h a c -> a c
preimage Hash {..} =
  let Payloaded (Wit w) = hValue
      raw = fromContext $ embedW w :: a c
      b = hasher raw == hHash
   in fromContext $
      ( fromCircuit2F (toContext raw) (toContext b) $ \r (Par1 i) -> do
          constraint (($ i) - one)
          return r
      )
