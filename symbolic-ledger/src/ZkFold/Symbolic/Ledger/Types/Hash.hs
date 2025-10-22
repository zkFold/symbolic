module ZkFold.Symbolic.Ledger.Types.Hash (
  Hash,
  HashSimple,
  hashFn,
) where

import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash qualified as Symbolic.Hash
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Class (Symbolic)
import qualified ZkFold.Symbolic.Algorithm.Hash.MiMC as MiMC

-- | Hash type used in the zkFold ledger.
type Hash = Symbolic.Hash.Hash HashSimple

-- TODO: Introduce a phantom type to track hash relation? Also should likely add strong typing than synonyms.

-- | Simplified hash type, that is just synonym of field element.
type HashSimple = FieldElement

hashFn :: (SymbolicData x, Symbolic c) => x c -> FieldElement c
hashFn =
  -- TODO: (#730) Move to Poseidon hash.
  MiMC.hash