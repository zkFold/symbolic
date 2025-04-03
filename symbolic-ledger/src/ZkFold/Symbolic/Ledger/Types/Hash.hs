{-# LANGUAGE UndecidableInstances #-}
module ZkFold.Symbolic.Ledger.Types.Hash (
  Hash,
  HashSimple,
 ) where

import           Prelude                           hiding (Bool, Eq, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Data.Class        (SymbolicData (Context))
import           ZkFold.Symbolic.Data.FieldElement (FieldElement)
import qualified ZkFold.Symbolic.Data.Hash         as Symbolic.Hash

-- | Hash type used in the zkFold ledger.
type Hash a = Symbolic.Hash.Hash (FieldElement (Context a)) a

-- | Simplified hash type, that is just synonym of field element.
type HashSimple c = FieldElement c
