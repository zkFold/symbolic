{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Symbolic.Data.Hash where

import Control.Monad (return)
import Data.Function (($))
import qualified GHC.Generics as G

import ZkFold.Algebra.Class
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Symbolic.Class (Symbolic, fromCircuit2F)
import ZkFold.Symbolic.Data.Bool (Bool (..), SymbolicEq)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Payloaded (Payloaded (..), payloaded, restored)
import ZkFold.Symbolic.MonadCircuit (constraint)

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
  { hHash :: h c
  , hValue :: Payloaded a c
  }
  deriving (G.Generic, G.Generic1, SymbolicData, SymbolicInput)

instance (Symbolic c, SymbolicEq h c) => Eq (Hash h a c)

-- | Restorably hash the data.
hash :: (Hashable (h c) (a c), SymbolicData a, Symbolic c) => a c -> Hash h a c
hash a = hasher a `Hash` payloaded a

-- | Restore the data which were hashed.
preimage
  :: forall h a c
   . ( Hashable (h c) (a c)
     , SymbolicData a
     , SymbolicEq h c
     , Symbolic c
     )
  => Hash h a c
  -> a c
preimage Hash {..} =
  let raw = restored hValue
      Bool b = hasher raw == hHash
   in restore
        ( fromCircuit2F (arithmetize raw) b $ \r (G.Par1 i) -> do
            constraint (($ i) - one)
            return r
        , payload raw
        )
