{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE PolyKinds #-}

module ZkFold.UPLC.BuiltinFunction where

import           ZkFold.UPLC.BuiltinType (BuiltinType (..))

-- | Builtin functions available on Cardano network.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf) (accessed in Nov 2024)
--
-- Things to note:
--
-- 1. Each builtin function tag is annotated with the type signature of a
--    corresponding function to avoid implementation errors.
-- 2. Monomorphic and polymorphic functions are treated differently in the
--    Converter so here they are split in two types for convenience.
data BuiltinFunction s t
    -- | Monomorphic builtin functions.
  = BFMono (BuiltinMonoFunction s t)
    -- | Polymorphic builtin functions.
  | BFPoly (BuiltinPolyFunction s t)

-- | Builtin monomorphic functions available on Cardano network.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf) (accessed in Nov 2024)
--
-- Things to note:
--
-- 1. Each builtin function tag is annotated with the type signature of a
--    corresponding function to avoid implementation errors.
-- 2. Monomorphic functions are split in groups according to the builtin type
--    in question for simpler incremental implementation of a Converter.
data BuiltinMonoFunction (s :: [BuiltinType]) (t :: BuiltinType)
  = BMFInteger (BuiltinIntegerFunction s t)
  | BMFByteString (BuiltinByteStringFunction s t)
  | BMFString (BuiltinStringFunction s t)
  | BMFAlgorithm (BuiltinAlgorithm s t)
  | BMFData (BuiltinDataFunction s t)
    -- | Available since Batch 4
  | BMFCurve (BuiltinBLSFunction s t)
    -- | Available since Batch 5
  | BMFBitwise (BuiltinBitwiseFunction s t)

-- | Builtin polymorphic functions available on Cardano network.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf) (accessed in Nov 2024)
--
-- Things to note:
--
-- 1. Each builtin function tag is annotated with the type signature of a
--    corresponding function for documentation purposes, as it is simpler to
--    check correctness of a few polymorphic function implementations than
--    write a dedicated type inference algorithm.
-- 2. Polymorphic List functions are grouped together for simpler incremental
--    implementation of a Converter.
data BuiltinPolyFunction (s :: [BuiltinType]) (t :: BuiltinType) where
  IfThenElse :: BuiltinPolyFunction '[BTBool, t, t] t
  ChooseUnit :: BuiltinPolyFunction '[BTUnit, t] t
  Trace :: BuiltinPolyFunction '[BTString, t] t
  FstPair :: BuiltinPolyFunction '[BTPair s t] s
  SndPair :: BuiltinPolyFunction '[BTPair s t] t
  BPFList :: BuiltinListFunction s t -> BuiltinPolyFunction s t
  ChooseData :: BuiltinPolyFunction '[BTData, t, t, t, t, t] t

-- | Builtin monomorphic Integer functions available on Cardano network.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf) (accessed in Nov 2024)
--
-- Note that each builtin function tag is annotated with the type signature of a
-- corresponding function to avoid implementation errors.
data BuiltinIntegerFunction (s :: [BuiltinType]) (t :: BuiltinType) where
  AddInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTInteger
  SubtractInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTInteger
  MultiplyInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTInteger
  DivideInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTInteger
  ModInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTInteger
  QuotientInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTInteger
  RemainderInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTInteger
  EqualsInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTBool
  LessThanInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTBool
  LessThanEqualsInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTBool
  -- | Available since Batch 4
  IntegerToByteString :: BuiltinIntegerFunction '[BTBool, BTInteger, BTInteger] BTByteString
  -- | Available since Batch 4
  ByteStringToInteger :: BuiltinIntegerFunction '[BTBool, BTByteString] BTInteger

-- | Builtin monomorphic ByteString functions available on Cardano network.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf) (accessed in Nov 2024)
--
-- Note that each builtin function tag is annotated with the type signature of a
-- corresponding function to avoid implementation errors.
data BuiltinByteStringFunction (s :: [BuiltinType]) (t :: BuiltinType) where
  AppendByteString :: BuiltinByteStringFunction '[BTByteString, BTByteString] BTByteString
  ConsByteString :: BuiltinByteStringFunction '[BTInteger, BTByteString] BTByteString
  SliceByteString :: BuiltinByteStringFunction '[BTInteger, BTInteger, BTByteString] BTByteString
  LengthOfByteString :: BuiltinByteStringFunction '[BTByteString] BTInteger
  IndexByteString :: BuiltinByteStringFunction '[BTByteString, BTInteger] BTInteger
  EqualsByteString :: BuiltinByteStringFunction '[BTByteString, BTByteString] BTBool
  LessThanByteString :: BuiltinByteStringFunction '[BTByteString, BTByteString] BTBool
  LessThanEqualsByteString :: BuiltinByteStringFunction '[BTByteString, BTByteString] BTBool

-- | Builtin monomorphic String functions available on Cardano network.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf) (accessed in Nov 2024)
--
-- Note that each builtin function tag is annotated with the type signature of a
-- corresponding function to avoid implementation errors.
data BuiltinStringFunction (s :: [BuiltinType]) (t :: BuiltinType) where
  AppendString :: BuiltinStringFunction '[BTString, BTString] BTString
  EqualsString :: BuiltinStringFunction '[BTString, BTString] BTBool
  EncodeUtf8 :: BuiltinStringFunction '[BTString] BTByteString
  DecodeUtf8 :: BuiltinStringFunction '[BTByteString] BTString

-- | Builtin monomorphic cryptographic functions available on Cardano network.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf) (accessed in Nov 2024)
--
-- Note that each builtin function tag is annotated with the type signature of a
-- corresponding function to avoid implementation errors.
data BuiltinAlgorithm (s :: [BuiltinType]) (t :: BuiltinType) where
  SHA2_256 :: BuiltinAlgorithm '[BTByteString] BTByteString
  SHA3_256 :: BuiltinAlgorithm '[BTByteString] BTByteString
  Blake2b_256 :: BuiltinAlgorithm '[BTByteString] BTByteString
  VerifyEd25519Signature :: BuiltinAlgorithm '[BTByteString, BTByteString, BTByteString] BTBool
  -- | Available since Batch 3
  VerifyEcdsaSecp256k1Signature :: BuiltinAlgorithm '[BTByteString, BTByteString, BTByteString] BTBool
  -- | Available since Batch 3
  VerifySchnorrSecp256k1Signature :: BuiltinAlgorithm '[BTByteString, BTByteString, BTByteString] BTBool
  -- | Available since Batch 4
  Blake2b_224 :: BuiltinAlgorithm '[BTByteString] BTByteString
  -- | Available since Batch 4
  Keccak_256 :: BuiltinAlgorithm '[BTByteString] BTByteString
  -- | Available since Batch 5
  Ripemd_160 :: BuiltinAlgorithm '[BTByteString] BTByteString

-- | Builtin polymorphic List functions available on Cardano network.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf) (accessed in Nov 2024)
--
-- Note that each builtin function tag is annotated with the type signature of a
-- corresponding function for documentation purposes, as it is simpler to
-- check correctness of a few polymorphic function implementations than
-- write a dedicated type inference algorithm.
data BuiltinListFunction (s :: [BuiltinType]) (t :: BuiltinType) where
  ChooseList :: BuiltinListFunction '[BTList s, t, t] t
  MkCons :: BuiltinListFunction '[t, BTList t] (BTList t)
  HeadList :: BuiltinListFunction '[BTList t] t
  TailList :: BuiltinListFunction '[BTList t] (BTList t)
  NullList :: BuiltinListFunction '[BTList t] BTBool

-- | Builtin monomorphic Data functions available on Cardano network.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf) (accessed in Nov 2024)
--
-- Note that each builtin function tag is annotated with the type signature of a
-- corresponding function to avoid implementation errors.
data BuiltinDataFunction (s :: [BuiltinType]) (t :: BuiltinType) where
  ConstrData :: BuiltinDataFunction '[BTInteger, BTList BTData] BTData
  MapData :: BuiltinDataFunction '[BTList (BTPair BTData BTData)] BTData
  ListData :: BuiltinDataFunction '[BTList BTData] BTData
  IData :: BuiltinDataFunction '[BTInteger] BTData
  BData :: BuiltinDataFunction '[BTByteString] BTData
  UnConstrData :: BuiltinDataFunction '[BTData] (BTPair BTInteger (BTList BTData))
  UnMapData :: BuiltinDataFunction '[BTData] (BTList (BTPair BTData BTData))
  UnListData :: BuiltinDataFunction '[BTData] (BTList BTData)
  UnIData :: BuiltinDataFunction '[BTData] BTInteger
  UnBData :: BuiltinDataFunction '[BTData] BTByteString
  EqualsData :: BuiltinDataFunction '[BTData, BTData] BTBool
  MkPairData :: BuiltinDataFunction '[BTData, BTData] (BTPair BTData BTData)
  MkNilData :: BuiltinDataFunction '[BTUnit] (BTList BTData)
  MkNilPairData :: BuiltinDataFunction '[BTUnit] (BTList (BTPair BTData BTData))
  -- | Available since Batch 2
  SerializeData :: BuiltinDataFunction '[BTData] BTByteString

-- | Builtin monomorphic BLS functions.
-- Available on Cardano network since Batch 4.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf) (accessed in Nov 2024)
--
-- Things to note:
--
-- 1. Each builtin function tag is annotated with the type signature of a
--    corresponding function to avoid implementation errors.
-- 2. Operations over different curves are grouped in separate datatypes.
data BuiltinBLSFunction (s :: [BuiltinType]) (t :: BuiltinType) where
  BLS_G1 :: BuiltinBLSG1Function s t -> BuiltinBLSFunction s t
  BLS_G2 :: BuiltinBLSG2Function s t -> BuiltinBLSFunction s t
  Bls12_381_millerLoop :: BuiltinBLSFunction '[BTBLSG1, BTBLSG2] BTBLSMLResult
  Bls12_381_mulMlResult :: BuiltinBLSFunction '[BTBLSMLResult, BTBLSMLResult] BTBLSMLResult
  Bls12_381_finalVerify :: BuiltinBLSFunction '[BTBLSMLResult, BTBLSMLResult] BTBool

-- | Builtin monomorphic BLS UNtwisted curve functions.
-- Available on Cardano network since Batch 4.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf) (accessed in Nov 2024)
--
-- Note that each builtin function tag is annotated with the type signature of a
-- corresponding function to avoid implementation errors.
data BuiltinBLSG1Function (s :: [BuiltinType]) (t :: BuiltinType) where
  Bls12_381_G1_add :: BuiltinBLSG1Function '[BTBLSG1, BTBLSG1] BTBLSG1
  Bls12_381_G1_neg :: BuiltinBLSG1Function '[BTBLSG1] BTBLSG1
  Bls12_381_G1_scalarMul :: BuiltinBLSG1Function '[BTInteger, BTBLSG1] BTBLSG1
  Bls12_381_G1_equal :: BuiltinBLSG1Function '[BTBLSG1, BTBLSG1] BTBool
  Bls12_381_G1_hashToGroup :: BuiltinBLSG1Function '[BTByteString, BTByteString] BTBLSG1
  Bls12_381_G1_compress :: BuiltinBLSG1Function '[BTBLSG1] BTByteString
  Bls12_381_G1_uncompress :: BuiltinBLSG1Function '[BTByteString] BTBLSG1

-- | Builtin monomorphic BLS twisted curve functions.
-- Available on Cardano network since Batch 4.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf) (accessed in Nov 2024)
--
-- Note that each builtin function tag is annotated with the type signature of a
-- corresponding function to avoid implementation errors.
data BuiltinBLSG2Function (s :: [BuiltinType]) (t :: BuiltinType) where
  Bls12_381_G2_add :: BuiltinBLSG2Function '[BTBLSG2, BTBLSG2] BTBLSG2
  Bls12_381_G2_neg :: BuiltinBLSG2Function '[BTBLSG2] BTBLSG2
  Bls12_381_G2_scalarMul :: BuiltinBLSG2Function '[BTInteger, BTBLSG2] BTBLSG2
  Bls12_381_G2_equal :: BuiltinBLSG2Function '[BTBLSG2, BTBLSG2] BTBool
  Bls12_381_G2_hashToGroup :: BuiltinBLSG2Function '[BTByteString, BTByteString] BTBLSG2
  Bls12_381_G2_compress :: BuiltinBLSG2Function '[BTBLSG2] BTByteString
  Bls12_381_G2_uncompress :: BuiltinBLSG2Function '[BTByteString] BTBLSG2

-- | Builtin monomorphic ByteString functions for bitwise operations.
-- Available on Cardano network since Batch 5.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf) (accessed in Nov 2024)
--
-- Note that each builtin function tag is annotated with the type signature of a
-- corresponding function to avoid implementation errors.
data BuiltinBitwiseFunction (s :: [BuiltinType]) (t :: BuiltinType) where
  AndByteString :: BuiltinBitwiseFunction '[BTBool, BTByteString, BTByteString] BTByteString
  OrByteString :: BuiltinBitwiseFunction '[BTBool, BTByteString, BTByteString] BTByteString
  XorByteString :: BuiltinBitwiseFunction '[BTBool, BTByteString, BTByteString] BTByteString
  ComplementByteString :: BuiltinBitwiseFunction '[BTByteString] BTByteString
  ShiftByteString :: BuiltinBitwiseFunction '[BTByteString, BTInteger] BTByteString
  RotateByteString :: BuiltinBitwiseFunction '[BTByteString, BTInteger] BTByteString
  CountSetBits :: BuiltinBitwiseFunction '[BTByteString] BTInteger
  FindFirstSetBit :: BuiltinBitwiseFunction '[BTByteString] BTInteger
  ReadBit :: BuiltinBitwiseFunction '[BTByteString, BTInteger] BTByteString
  WriteBits :: BuiltinBitwiseFunction '[BTByteString, BTList BTInteger, BTBool] BTByteString
  ReplicateByte :: BuiltinBitwiseFunction '[BTInteger, BTInteger] BTByteString
