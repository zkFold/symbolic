module ZkFold.Symbolic.Ledger.Types.Hash (
  Hash,
  HashSimple,
  hashFn,
) where

import ZkFold.Data.Collect (Collect)
import ZkFold.Symbolic.Algorithm.Hash.MiMC qualified as MiMC
import ZkFold.Symbolic.Compat (CompatData)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash qualified as Symbolic.Hash
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import ZkFold.Symbolic.V2 (Symbolic)

-- | Hash type used in the zkFold ledger.
type Hash = Symbolic.Hash.Hash HashSimple

-- TODO: Introduce a phantom type to track hash relation? Also should likely add strong typing than synonyms.

-- | Simplified hash type, that is just synonym of field element.
type HashSimple = CompatData FieldElement

hashFn :: (Collect (ConstrainedDatum c) x, Symbolic c) => x -> HashSimple c
hashFn =
  -- TODO: (#730) Move to Poseidon hash.
  MiMC.hashV2
