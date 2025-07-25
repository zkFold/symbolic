{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module ZkFold.UPLC.BuiltinFunction where

import ZkFold.UPLC.BuiltinType (BuiltinType (..))
import qualified Flat.Decoder as Flat
import Control.Monad ((>>=), fail)
import Data.Function (($))

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
  = -- | Monomorphic builtin functions.
    BFMono (BuiltinMonoFunction s t)
  | -- | Polymorphic builtin functions.
    BFPoly (BuiltinPolyFunction s t)

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
  | -- | Available since Batch 4
    BMFCurve (BuiltinBLSFunction s t)
  | -- | Available since Batch 5
    BMFBitwise (BuiltinBitwiseFunction s t)

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

-- | A decoder of a 'BuiltinFunction', according to
-- [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf)
-- (accessed in Jul 2025).
getBuiltinFunction :: (forall s t. BuiltinFunction s t -> Flat.Get r) -> Flat.Get r
getBuiltinFunction k = Flat.dBEBits8 7 >>= \case
  0b0000000 -> k $ BFMono (BMFInteger AddInteger)
  0b0000001 -> k $ BFMono (BMFInteger SubtractInteger)
  0b0000010 -> k $ BFMono (BMFInteger MultiplyInteger)
  0b0000011 -> k $ BFMono (BMFInteger DivideInteger)
  0b0000100 -> k $ BFMono (BMFInteger QuotientInteger)
  0b0000101 -> k $ BFMono (BMFInteger RemainderInteger)
  0b0000110 -> k $ BFMono (BMFInteger ModInteger)
  0b0000111 -> k $ BFMono (BMFInteger EqualsInteger)
  0b0001000 -> k $ BFMono (BMFInteger LessThanInteger)
  0b0001001 -> k $ BFMono (BMFInteger LessThanEqualsInteger)
  0b0001010 -> k $ BFMono (BMFByteString AppendByteString)
  0b0001011 -> k $ BFMono (BMFByteString ConsByteString)
  0b0001100 -> k $ BFMono (BMFByteString SliceByteString)
  0b0001101 -> k $ BFMono (BMFByteString LengthOfByteString)
  0b0001110 -> k $ BFMono (BMFByteString IndexByteString)
  0b0001111 -> k $ BFMono (BMFByteString EqualsByteString)
  0b0010000 -> k $ BFMono (BMFByteString LessThanByteString)
  0b0010001 -> k $ BFMono (BMFByteString LessThanEqualsByteString)
  0b0010010 -> k $ BFMono (BMFAlgorithm SHA2_256)
  0b0010011 -> k $ BFMono (BMFAlgorithm SHA3_256)
  0b0010100 -> k $ BFMono (BMFAlgorithm Blake2b_256)
  0b0010101 -> k $ BFMono (BMFAlgorithm VerifyEd25519Signature)
  0b0010110 -> k $ BFMono (BMFString AppendString)
  0b0010111 -> k $ BFMono (BMFString EqualsString)
  0b0011000 -> k $ BFMono (BMFString EncodeUtf8)
  0b0011001 -> k $ BFMono (BMFString DecodeUtf8)
  0b0011010 -> k $ BFPoly IfThenElse
  0b0011011 -> k $ BFPoly ChooseUnit
  0b0011100 -> k $ BFPoly Trace
  0b0011101 -> k $ BFPoly FstPair
  0b0011110 -> k $ BFPoly SndPair
  0b0011111 -> k $ BFPoly (BPFList ChooseList)
  0b0100000 -> k $ BFPoly (BPFList MkCons)
  0b0100001 -> k $ BFPoly (BPFList HeadList)
  0b0100010 -> k $ BFPoly (BPFList TailList)
  0b0100011 -> k $ BFPoly (BPFList NullList)
  0b0100100 -> k $ BFPoly ChooseData
  0b0100101 -> k $ BFMono (BMFData ConstrData)
  0b0100110 -> k $ BFMono (BMFData MapData)
  0b0100111 -> k $ BFMono (BMFData ListData)
  0b0101000 -> k $ BFMono (BMFData IData)
  0b0101001 -> k $ BFMono (BMFData BData)
  0b0101010 -> k $ BFMono (BMFData UnConstrData)
  0b0101011 -> k $ BFMono (BMFData UnMapData)
  0b0101100 -> k $ BFMono (BMFData UnListData)
  0b0101101 -> k $ BFMono (BMFData UnIData)
  0b0101110 -> k $ BFMono (BMFData UnBData)
  0b0101111 -> k $ BFMono (BMFData EqualsData)
  0b0110000 -> k $ BFMono (BMFData MkPairData)
  0b0110001 -> k $ BFMono (BMFData MkNilData)
  0b0110010 -> k $ BFMono (BMFData MkNilPairData)
  0b0110011 -> k $ BFMono (BMFData SerializeData)
  0b0110100 -> k $ BFMono (BMFAlgorithm VerifyEcdsaSecp256k1Signature)
  0b0110101 -> k $ BFMono (BMFAlgorithm VerifySchnorrSecp256k1Signature)
  0b0110110 -> k $ BFMono $ BMFCurve (BLS_G1 Bls12_381_G1_add)
  0b0110111 -> k $ BFMono $ BMFCurve (BLS_G1 Bls12_381_G1_neg)
  0b0111000 -> k $ BFMono $ BMFCurve (BLS_G1 Bls12_381_G1_scalarMul)
  0b0111001 -> k $ BFMono $ BMFCurve (BLS_G1 Bls12_381_G1_equal)
  0b0111010 -> k $ BFMono $ BMFCurve (BLS_G1 Bls12_381_G1_hashToGroup)
  0b0111011 -> k $ BFMono $ BMFCurve (BLS_G1 Bls12_381_G1_compress)
  0b0111100 -> k $ BFMono $ BMFCurve (BLS_G1 Bls12_381_G1_uncompress)
  0b0111101 -> k $ BFMono $ BMFCurve (BLS_G2 Bls12_381_G2_add)
  0b0111110 -> k $ BFMono $ BMFCurve (BLS_G2 Bls12_381_G2_neg)
  0b0111111 -> k $ BFMono $ BMFCurve (BLS_G2 Bls12_381_G2_scalarMul)
  0b1000000 -> k $ BFMono $ BMFCurve (BLS_G2 Bls12_381_G2_equal)
  0b1000001 -> k $ BFMono $ BMFCurve (BLS_G2 Bls12_381_G2_hashToGroup)
  0b1000010 -> k $ BFMono $ BMFCurve (BLS_G2 Bls12_381_G2_compress)
  0b1000011 -> k $ BFMono $ BMFCurve (BLS_G2 Bls12_381_G2_uncompress)
  0b1000100 -> k $ BFMono (BMFCurve Bls12_381_millerLoop)
  0b1000101 -> k $ BFMono (BMFCurve Bls12_381_mulMlResult)
  0b1000110 -> k $ BFMono (BMFCurve Bls12_381_finalVerify)
  0b1000111 -> k $ BFMono (BMFAlgorithm Keccak_256)
  0b1001000 -> k $ BFMono (BMFAlgorithm Blake2b_224)
  0b1001001 -> k $ BFMono (BMFInteger IntegerToByteString)
  0b1001010 -> k $ BFMono (BMFInteger ByteStringToInteger)
  0b1001011 -> k $ BFMono (BMFBitwise AndByteString)
  0b1001100 -> k $ BFMono (BMFBitwise OrByteString)
  0b1001101 -> k $ BFMono (BMFBitwise XorByteString)
  0b1001110 -> k $ BFMono (BMFBitwise ComplementByteString)
  0b1001111 -> k $ BFMono (BMFBitwise ReadBit)
  0b1010000 -> k $ BFMono (BMFBitwise WriteBits)
  0b1010001 -> k $ BFMono (BMFBitwise ReplicateByte)
  0b1010010 -> k $ BFMono (BMFBitwise ShiftByteString)
  0b1010011 -> k $ BFMono (BMFBitwise RotateByteString)
  0b1010100 -> k $ BFMono (BMFBitwise CountSetBits)
  0b1010101 -> k $ BFMono (BMFBitwise FindFirstSetBit)
  0b1010110 -> k $ BFMono (BMFAlgorithm Ripemd_160)
  _ -> fail "unknown builtin function"
