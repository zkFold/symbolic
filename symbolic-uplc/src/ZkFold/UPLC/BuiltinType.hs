{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module ZkFold.UPLC.BuiltinType where

import Data.Function ((.))

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
  | -- | Batch 4
    BTBLSG1
  | -- | Batch 4
    BTBLSG2
  | -- | Batch 4
    BTBLSMLResult

-- | Reflection of type-level 'BuiltinType' onto term-level.
data DemotedType t where
  DInteger :: DemotedType BTInteger
  DByteString :: DemotedType BTByteString
  DString :: DemotedType BTString
  DBool :: DemotedType BTBool
  DUnit :: DemotedType BTUnit
  DData :: DemotedType BTData
  DemList :: DemotedType t -> DemotedType (BTList t)
  DPair :: DemotedType t -> DemotedType u -> DemotedType (BTPair t u)
  DG1 :: DemotedType BTBLSG1
  DG2 :: DemotedType BTBLSG2
  DGR :: DemotedType BTBLSMLResult

-- | Lifting of the term-level 'BuiltinType' to type-level via 'DemotedType'.
demoted :: (forall t. DemotedType t -> r) -> BuiltinType -> r
demoted k = \case
  BTInteger -> k DInteger
  BTByteString -> k DByteString
  BTString -> k DString
  BTBool -> k DBool
  BTUnit -> k DUnit
  BTData -> k DData
  BTList u -> demoted (k . DemList) u
  BTPair u v -> demoted (\w -> demoted (k . DPair w) v) u
  BTBLSG1 -> k DG1
  BTBLSG2 -> k DG2
  BTBLSMLResult -> k DGR
