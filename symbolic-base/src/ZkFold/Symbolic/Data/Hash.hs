{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Symbolic.Data.Hash where

import Control.Monad (return)
import Data.Bifunctor (bimap)
import Data.Function (const, ($), (.))
import Data.Functor ((<$>))
import Data.Traversable (traverse)
import qualified GHC.Generics as G

import ZkFold.Algebra.Class
import ZkFold.Control.HApplicative (hunit)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Symbolic.Algorithm.Interpolation (interpolateW)
import ZkFold.Symbolic.Class (Symbolic, BaseField, fromCircuitF, witnessF, fromCircuit2F)
import ZkFold.Symbolic.Data.Bool (Bool (..), SymbolicEq)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Payloaded (Payloaded (..))
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

newtype PayloadedData x c = PayloadedData
  { runPayloadedData ::
        Payloaded (Layout x (Order (BaseField c)) G.:*: Payload x (Order (BaseField c))) c
  }

instance SymbolicData x => SymbolicData (PayloadedData x) where
  type Layout (PayloadedData x) _ = G.U1
  type Payload (PayloadedData x) n = Layout x n G.:*: Payload x n
  arithmetize _ = hunit
  payload = runPayloaded . runPayloadedData
  interpolate bs (witnessF -> G.Par1 c) =
    PayloadedData $ Payloaded $ interpolateW (bimap fromConstant (runPayloaded . runPayloadedData) <$> bs) c
  restore = _

instance SymbolicData x => SymbolicInput (PayloadedData x) where

-- | An invertible hash 'h' of a symbolic datatype 'a'.
data Hash h a c = Hash
  { hHash :: h c
  , hValue :: PayloadedData a c
  }
  deriving (G.Generic, G.Generic1, SymbolicData, SymbolicInput )

instance (Symbolic c, SymbolicData a, SymbolicEq h c) => Eq (Hash h a c)

-- | Restorably hash the data.
hash :: (Hashable (h c) (a c), SymbolicData a, Symbolic c) => a c -> Hash h a c
hash a =
  Hash (hasher a) $
    PayloadedData $ Payloaded (witnessF (arithmetize a) G.:*: payload a)

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
  let Payloaded (l G.:*: p) = runPayloadedData hValue
      raw :: a c =
        restore
          ( fromCircuitF hunit $ const (traverse unconstrained l)
          , p
          )
      Bool b = hasher raw == hHash
   in restore
        ( fromCircuit2F (arithmetize raw) b $ \r (G.Par1 i) -> do
            constraint (($ i) - one)
            return r
        , payload raw
        )
