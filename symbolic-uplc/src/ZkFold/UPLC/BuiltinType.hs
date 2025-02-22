module ZkFold.UPLC.BuiltinType where

-- | Builtin types available on Cardano network.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf)
-- (accessed in Nov 2024)
data BuiltinType
  = BTInteger
  | BTByteString
  | BTString
  | BTBool
  | BTUnit
  | BTData
  | BTList BuiltinType
  | BTPair BuiltinType BuiltinType
  | BTBLSG1 -- ^ Batch 4
  | BTBLSG2 -- ^ Batch 4
  | BTBLSMLResult -- ^ Batch 4
