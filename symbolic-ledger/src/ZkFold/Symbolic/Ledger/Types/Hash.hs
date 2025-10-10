module ZkFold.Symbolic.Ledger.Types.Hash (
  Hash,
  HashSimple,
) where

import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash qualified as Symbolic.Hash

-- | Hash type used in the zkFold ledger.
type Hash = Symbolic.Hash.Hash HashSimple

-- TODO: Introduce a phantom type to track hash relation? Also should likely add strong typing than synonyms.

-- | Simplified hash type, that is just synonym of field element.
type HashSimple = FieldElement
