{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Hash where

import Control.Monad (return)
import Data.Function (const, ($))
import Data.Proxy (Proxy (..))
import Data.Traversable (traverse)
import Data.Type.Equality (type (~))
import GHC.Generics (Generic, Par1 (..), (:*:) (..))

import ZkFold.Algebra.Class
import ZkFold.Control.HApplicative (hunit)
import ZkFold.Symbolic.Class (Symbolic (fromCircuitF, witnessF), fromCircuit2F)
import ZkFold.Symbolic.Data.Bool (Bool (..))
import ZkFold.Symbolic.Data.Class (SymbolicData (..), SymbolicOutput)
import ZkFold.Symbolic.Data.Conditional (Conditional)
import ZkFold.Symbolic.Data.Eq (Eq (..), SymbolicEq, (==))
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

instance (SymbolicOutput a, SymbolicOutput h) => SymbolicData (Hash h a)

instance (SymbolicInput a, SymbolicInput h) => SymbolicInput (Hash h a)

instance (Conditional (Bool c) h, Symbolic c, SymbolicData a, c ~ (Context h)) => Conditional (Bool c) (Hash h a)

instance (BooleanOf h ~ Bool c, Eq h, Symbolic c, SymbolicData a, c ~ (Context h)) => Eq (Hash h a)

-- | Restorably hash the data.
hash :: (Context h ~ Context a, Hashable h a, SymbolicOutput a) => a -> Hash h a
hash a =
  Hash (hasher a) $
    Payloaded (witnessF (arithmetize a Proxy) :*: payload a Proxy)

-- | Restore the data which were hashed.
preimage
  :: forall h a c
   . ( Context a ~ c
     , Context h ~ c
     , Hashable h a
     , SymbolicEq h
     , SymbolicOutput a
     )
  => Hash h a -> a
preimage Hash {..} =
  let Payloaded (l :*: p) = hValue
      raw :: a =
        restore $
          const
            ( fromCircuitF hunit $ const (traverse unconstrained l)
            , p
            )
      Bool b = hasher raw == hHash
   in restore $ \s ->
        ( fromCircuit2F (arithmetize raw s) b $ \r (Par1 i) -> do
            constraint (($ i) - one)
            return r
        , payload raw s
        )
