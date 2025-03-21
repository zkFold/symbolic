{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE UndecidableInstances #-}
module ZkFold.Symbolic.Ledger.Types.Hash (
  Hash,
  Hashable (..),
 ) where

import           Prelude                           hiding (Bool, Eq, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Class             (Symbolic)
import           ZkFold.Symbolic.Data.Bool         (Bool)
import           ZkFold.Symbolic.Data.Class        (SymbolicData)
import           ZkFold.Symbolic.Data.Conditional  (Conditional)
import           ZkFold.Symbolic.Data.Eq           (Eq)
import           ZkFold.Symbolic.Data.FieldElement (FieldElement)

-- TODO: Use underlying Hash type from symbolic-base, track: https://github.com/zkFold/symbolic/issues/524.
-- | Hash type used in the zkFold ledger.
newtype Hash context = Hash (FieldElement context)

deriving newtype instance (Symbolic context) => SymbolicData (Hash context)
deriving newtype instance (Symbolic context) => Conditional (Bool context) (Hash context)
deriving newtype instance (Symbolic context) => Eq (Hash context)

class Hashable context x where
  hash :: x -> Hash context
