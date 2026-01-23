{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Symbolic.Cardano.Contracts.ZkPass where

import ZkFold.ArithmeticCircuit.Op (Sort (..))
import           GHC.Generics                            ((:*:) (..), U1, Par1, Generic, Generic1)
import           GHC.TypeLits                            (KnownNat)
import           Prelude                                 hiding (Bool, Eq (..), concat, head, length, splitAt, (!!),
                                                          (&&), (*), (+))

import           ZkFold.Algebra.Class                    (zero, Order)
import           ZkFold.Algebra.EllipticCurve.Class      hiding (Point)
import ZkFold.Symbolic.Class (BaseField, Symbolic (..))
import ZkFold.ArithmeticCircuit.Node qualified as C
import qualified ZkFold.Data.Vector                      as V
import           ZkFold.Data.Vector                      hiding (concat, append)
import           ZkFold.Symbolic.Algorithm.ECDSA.ECDSA   (ecdsaVerify)
import           ZkFold.Symbolic.Algorithm.Hash.Keccak   (Keccak, keccak)
import qualified ZkFold.Symbolic.Class                   as S
import           ZkFold.Data.Eq
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString         (ByteString, append, concat, dropN, toWords)
import           ZkFold.Symbolic.Data.Combinators        (GetRegisterSize, Iso (..), NumberOfRegisters, RegisterSize (..))
import           ZkFold.Symbolic.Data.EllipticCurve.Point (Point (..))
import           ZkFold.Symbolic.Data.FFA                (FFA, KnownFFA, unsafeFromUInt)
import           ZkFold.Symbolic.Data.FieldElement       (FieldElement)
import           ZkFold.Symbolic.Data.UInt               (UInt, toNative)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Symbolic.Data.Class (SymbolicData(..))
import ZkFold.ArithmeticCircuit (ArithmeticCircuit)
import ZkFold.Symbolic.Compat (CompatContext)
import ZkFold.Symbolic.Data.Input (SymbolicInput)

data ZKPassResult c = ZKPassResult
  { allocatorPublicKey :: ByteString 512 c
  , allocatorSignature :: ByteString 520 c
  , publicFields       :: ByteString 1024 c
  , publicFieldsHash   :: ByteString 256 c
  , taskId             :: ByteString 256 c
  , uHash              :: ByteString 256 c
  , validatorPublicKey :: ByteString 512 c
  , validatorSignature :: ByteString 520 c
  , schemaId           :: ByteString 256 c
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

-- | Convert a 256-bit ByteString to an FFA element
fromByteString256 :: forall p ctx .
    ( S.Symbolic ctx
    , KnownFFA p 'Auto ctx
    , KnownNat (GetRegisterSize (S.BaseField ctx) 256 'Auto)
    )
    => ByteString 256 ctx
    -> FFA p 'Auto ctx
fromByteString256 = unsafeFromUInt @256 . from

hashFunction :: forall n c .
    ( Keccak "Keccak256" c n
    )
    => ByteString n c
    -> ByteString 256 c
hashFunction = keccak @"Keccak256"

verifyAllocatorSignature :: forall n p q curve ctx.
     ( KnownNat n
     , KnownNat (NumberOfRegisters (S.BaseField ctx) n 'Auto)
     , KnownNat (GetRegisterSize (S.BaseField ctx) n 'Auto)
     , KnownNat (GetRegisterSize (S.BaseField ctx) 256 'Auto)
     , KnownFFA p 'Auto ctx
     , KnownFFA q 'Auto ctx
     , ScalarFieldOf (Point (Weierstrass curve) (FFA q 'Auto) ctx) ~ FFA p 'Auto ctx
     , CyclicGroup (Point (Weierstrass curve) (FFA q 'Auto) ctx)
     , Keccak "Keccak256" ctx 672
     , Keccak "Keccak256" ctx 512
     )
    => ByteString 256 ctx
    -> ByteString 512 ctx
    -> ByteString 512 ctx
    -> ByteString 520 ctx
    -> ByteString 256 ctx
    -> Bool ctx
verifyAllocatorSignature taskId validatorPublicKey allocatorPublicKey allocatorSignature schemaId = verifyVerdict
    where
        params :: ByteString 672 ctx
        params = (taskId `append` schemaId) `append` dropN @160 (keccak @"Keccak256" validatorPublicKey)

        (r, s, _) = extractSignature @p allocatorSignature

        (xVec, yVec) = splitAt (toWords allocatorPublicKey) :: (Vector 1 (ByteString 256 ctx), Vector 1 (ByteString 256 ctx))

        pubKey :: Point (Weierstrass curve) (FFA q 'Auto) ctx
        pubKey = pointXY (fromByteString256 $ item xVec) (fromByteString256 $ item yVec)

        verifyVerdict :: Bool ctx
        verifyVerdict = ecdsaVerify @_ @n (fromByteString256 . hashFunction) pubKey params (r :*: s)

verifyValidatorSignature :: forall n p q curve ctx.
     ( KnownNat n
     , KnownNat (NumberOfRegisters (S.BaseField ctx) n 'Auto)
     , KnownNat (GetRegisterSize (S.BaseField ctx) n 'Auto)
     , KnownNat (GetRegisterSize (S.BaseField ctx) 256 'Auto)
     , KnownFFA p 'Auto ctx
     , KnownFFA q 'Auto ctx
     , ScalarFieldOf (Point (Weierstrass curve) (FFA q 'Auto) ctx) ~ FFA p 'Auto ctx
     , CyclicGroup (Point (Weierstrass curve) (FFA q 'Auto) ctx)
     , Keccak "Keccak256" ctx 1024
     )
    => ByteString 256 ctx
    -> ByteString 256 ctx
    -> ByteString 256 ctx
    -> ByteString 512 ctx
    -> ByteString 520 ctx
    -> ByteString 256 ctx
    -> Bool ctx
verifyValidatorSignature taskId uHash publicFieldsHash validatorPublicKey validatorSignature schemaId = verifyVerdict
    where
        params :: ByteString 1024 ctx
        params = concat $ V.unsafeToVector [taskId, schemaId, uHash, publicFieldsHash]

        (r, s, _) = extractSignature @p validatorSignature

        (xVec, yVec) = splitAt (toWords validatorPublicKey) :: (Vector 1 (ByteString 256 ctx), Vector 1 (ByteString 256 ctx))

        pubKey :: Point (Weierstrass curve) (FFA q 'Auto) ctx
        pubKey = pointXY (fromByteString256 $ item xVec) (fromByteString256 $ item yVec)

        verifyVerdict :: Bool ctx
        verifyVerdict = ecdsaVerify @_ @n (fromByteString256 . hashFunction) pubKey params (r :*: s)

extractSignature :: forall p ctx .
    ( S.Symbolic ctx
    , KnownFFA p 'Auto ctx
    , KnownNat (GetRegisterSize (S.BaseField ctx) 256 'Auto)
    )
    => ByteString 520 ctx
    -> (FFA p 'Auto ctx, FFA p 'Auto ctx, ByteString 8 ctx)
extractSignature sign = (fromByteString256 $ concat r', fromByteString256 $ concat s', item v')
    where
        r' :: Vector 32 (ByteString 8 ctx)

        bytes = toWords sign

        (r', l') = splitAt bytes

        (s', v') = splitAt l'

type Signature n p q curve ctx =
     ( KnownNat n
     , KnownNat (NumberOfRegisters (S.BaseField ctx) n 'Auto)
     , KnownNat (GetRegisterSize (S.BaseField ctx) n 'Auto)
     , KnownNat (GetRegisterSize (S.BaseField ctx) 256 'Auto)
     , KnownFFA p 'Auto ctx
     , KnownFFA q 'Auto ctx
     , ScalarFieldOf (Point (Weierstrass curve) (FFA q 'Auto) ctx) ~ FFA p 'Auto ctx
     , CyclicGroup (Point (Weierstrass curve) (FFA q 'Auto) ctx)
     , Keccak "Keccak256" ctx 1024
     , Keccak "Keccak256" ctx 672
     , Keccak "Keccak256" ctx 512
     )

zkPassSymbolicVerifier :: forall n p q curve ctx. Signature n p q curve ctx =>
    ZKPassResult ctx
    -> FieldElement ctx
zkPassSymbolicVerifier (ZKPassResult
    allocatorPublicKey
    allocatorSignature
    publicFields
    publicFieldsHash
    taskId
    uHash
    validatorPublicKey
    validatorSignature
    schemaId
    ) =
    let
        conditionAllocatorSignatureCorrect = verifyAllocatorSignature @n @p @q @curve
            taskId validatorPublicKey allocatorPublicKey allocatorSignature schemaId

        conditionHashEquality = hashFunction publicFields == publicFieldsHash

        conditionValidatorSignatureCorrect = verifyValidatorSignature @n @p @q @curve
            taskId uHash publicFieldsHash validatorPublicKey validatorSignature schemaId

        allConditions = conditionAllocatorSignatureCorrect && conditionHashEquality && conditionValidatorSignatureCorrect

    in bool zero (toNative (from publicFieldsHash :: UInt 256 'Auto ctx)) allConditions

-----------------------------------------
-- Compilation
-----------------------------------------


type ZkPassBF = Zp BLS12_381_Scalar

type ZkPassContractInput = ZKPassResult

type ZkPassContractInputLayout =
  Layout
    (ZkPassContractInput :*: U1)
    (Order ZkPassBF)

type ZkPassContractInputPayload =
  Payload
    (ZkPassContractInput :*: U1)
    (Order ZkPassBF)

type ZkPassContractCompiledInput =
  ZkPassContractInputLayout :*: ZkPassContractInputPayload

type ZkPassContractOutputLayout = Par1

type ZkPassCircuit =
  ArithmeticCircuit ZkPassBF ZkPassContractCompiledInput ZkPassContractOutputLayout

type ZkPassContractOutput = FieldElement

zksPassCircuit
  :: forall n p q curve.
  ( Signature n p q curve (CompatContext (C.Node (Order ZkPassBF) ZZp))
  )
  => ZkPassCircuit
zksPassCircuit = C.compileV1 @ZkPassBF (zkPassSymbolicVerifier @n @p @q @curve)