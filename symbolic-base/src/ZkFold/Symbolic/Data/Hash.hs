{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Hash where

import Data.Function ((.))
import Data.Type.Equality (type (~))
import qualified GHC.Generics as G
import qualified Prelude as Haskell

import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool (..))
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import ZkFold.Symbolic.Data.Unconstrained

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
  , hValue :: Unconstrained a c
  }
  deriving (G.Generic, G.Generic1)

instance (SymbolicData h, SymbolicData a) => SymbolicData (Hash h a)

instance (SymbolicInput h, SymbolicData a) => SymbolicInput (Hash h a) where
  isValid = isValid . hHash -- FIXME: check validity of 'hValue'

instance Collect (ConstrainedDatum c) (h c) => Collect (ConstrainedDatum c) (Hash h a c)

instance Eq (h c) => Eq (Hash h a c) where
  type BooleanOf (Hash h a c) = BooleanOf (h c)
  Hash h _ == Hash h' _ = h == h'

deriving stock instance
  (Haskell.Show (h c), Haskell.Show (Unconstrained a c))
  => Haskell.Show (Hash h a c)

-- | Restorably hash the data.
hash :: (Hashable (h c) (a c), SymbolicData a, Symbolic c) => a c -> Hash h a c
hash a = hasher a `Hash` unconstrain a

-- | Restore the data which were hashed.
preimage
  :: forall h a c
   . ( Hashable (h c) (a c)
     , Eq (h c)
     , BooleanOf (h c) ~ Bool c
     , SymbolicData a
     , Symbolic c
     )
  => Hash h a c
  -> a c
preimage Hash {..} = constrained (\value -> hasher value == hHash) hValue
