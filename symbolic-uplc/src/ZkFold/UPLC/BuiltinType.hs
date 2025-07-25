{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module ZkFold.UPLC.BuiltinType where

import Data.Function ((.))
import qualified Flat.Decoder as Flat
import Control.Monad ((>>=), return, fail)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))

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

-- | A decoder of a 'BuiltinType', according to
-- [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf)
-- (accessed in Jul 2025).
getType :: Flat.Get BuiltinType
getType = tag >>= \case
  0b0000 -> return BTInteger
  0b0001 -> return BTByteString
  0b0010 -> return BTString
  0b0011 -> return BTUnit
  0b0100 -> return BTBool
  0b1000 -> return BTData
  0b1001 -> return BTBLSG1
  0b1010 -> return BTBLSG2
  0b1011 -> return BTBLSMLResult
  0b0111 -> tag >>= \case
    0b0101 -> BTList <$> getType
    0b0111 -> tag >>= \case
      0b0110 -> BTPair <$> getType <*> getType
      _ -> fail "unknown binary type constructor"
    _ -> fail "unknown unary type constructor"
  _ -> fail "unknown UPLC type"
  where tag = Flat.dBEBits8 4

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
