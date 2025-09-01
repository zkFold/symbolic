module ZkFold.Symbolic.Ledger.Types.Hash (
  Hash,
  HashSimple,
) where

import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import qualified ZkFold.Symbolic.Data.Hash as Symbolic.Hash

-- | Hash type used in the zkFold ledger.
type Hash = Symbolic.Hash.Hash FieldElement

-- TODO: Introduce a phantom type to track hash relation? Also should likely add strong typing than synonyms.

-- | Simplified hash type, that is just synonym of field element.
type HashSimple = FieldElement
