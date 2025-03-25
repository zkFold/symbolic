{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module ZkFold.Symbolic.Ledger.Types.Hash (
  Hash,
  Hashable (..),
  HashSimple,
 ) where

import           Prelude                           hiding (Bool, Eq, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Data.Bool         (Bool)
import           ZkFold.Symbolic.Data.Class        (SymbolicData (Context), SymbolicOutput)
import           ZkFold.Symbolic.Data.Conditional  (Conditional)
import           ZkFold.Symbolic.Data.Eq           (Eq)
import           ZkFold.Symbolic.Data.FieldElement (FieldElement)
import qualified ZkFold.Symbolic.Data.Hash         as Symbolic.Hash
import           ZkFold.Symbolic.Data.Input        (SymbolicInput)

-- | Hash type used in the zkFold ledger.
newtype Hash a = Hash (Symbolic.Hash.Hash (FieldElement (Context a)) a)

deriving newtype instance (SymbolicOutput a) => SymbolicData (Hash a)
deriving newtype instance (SymbolicInput a) => SymbolicInput (Hash a)
deriving newtype instance (c ~ Context a, SymbolicData a) => Conditional (Bool c) (Hash a)
deriving newtype instance (c ~ Context a, SymbolicData a) => Eq (Hash a)

class Hashable x where
  hash :: x -> Hash x

-- | Simplified hash type, that is just synonym of field element.
type HashSimple c = FieldElement c
