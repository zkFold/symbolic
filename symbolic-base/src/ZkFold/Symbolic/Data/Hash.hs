{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Hash where

import Control.Monad (return)
import Data.Function (const, ($))
import Data.Traversable (traverse)
import Data.Type.Equality (type (~))
import GHC.Generics (Generic, Par1 (..), (:*:) (..))

import ZkFold.Algebra.Class
import ZkFold.Control.HApplicative (hunit)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Symbolic.Class (Symbolic (fromCircuitF, witnessF), fromCircuit2F)
import ZkFold.Symbolic.Data.Bool (Bool (..), SymbolicEq)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Payloaded (Payloaded (Payloaded))
import ZkFold.Symbolic.MonadCircuit (constraint, unconstrained)

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
data Hash h a = Hash
  { hHash :: h
  , hValue :: Payloaded (Layout a :*: Payload a) (Context h)
  }
  deriving Generic

instance (SymbolicData h, SymbolicData a) => SymbolicData (Hash h a)

instance (SymbolicInput h, SymbolicInput a) => SymbolicInput (Hash h a)

instance (c ~ Context h, Symbolic c, SymbolicData a, BooleanOf h ~ Bool c, Eq h) => Eq (Hash h a)

-- | Restorably hash the data.
hash :: (Hashable h a, SymbolicData a, Context h ~ Context a) => a -> Hash h a
hash a =
  Hash (hasher a) $
    Payloaded (witnessF (arithmetize a) :*: payload a)

-- | Restore the data which were hashed.
preimage
  :: forall h a c
   . ( Hashable h a
     , SymbolicData a
     , Context h ~ c
     , Context a ~ c
     , SymbolicEq h
     )
  => Hash h a
  -> a
preimage Hash {..} =
  let Payloaded (l :*: p) = hValue
      raw :: a =
        restore
          ( fromCircuitF hunit $ const (traverse unconstrained l)
          , p
          )
      Bool b = hasher raw == hHash
   in restore
        ( fromCircuit2F (arithmetize raw) b $ \r (Par1 i) -> do
            constraint (($ i) - one)
            return r
        , payload raw
        )
