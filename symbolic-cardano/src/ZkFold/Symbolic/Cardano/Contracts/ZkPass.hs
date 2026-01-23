{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Cardano.Contracts.ZkPass where

import           GHC.Generics                            ((:*:) (..))
import           GHC.TypeLits                            (KnownNat)
import           Prelude                                 hiding (Bool, Eq (..), concat, head, length, splitAt, (!!),
                                                          (&&), (*), (+))

import           ZkFold.Algebra.EllipticCurve.Class      hiding (Point)
import qualified ZkFold.Data.Vector                      as V
import           ZkFold.Data.Vector                      hiding (concat)
import           ZkFold.Symbolic.Algorithm.ECDSA.ECDSA   (ecdsaVerify)
import           ZkFold.Symbolic.Algorithm.Hash.Keccak   (Keccak, keccak)
import qualified ZkFold.Symbolic.Class                   as S
import           ZkFold.Data.Eq
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString         (ByteString, concat, toWords)
import           ZkFold.Symbolic.Data.Combinators        (GetRegisterSize, Iso (..), NumberOfRegisters, RegisterSize (..), resize)
import           ZkFold.Symbolic.Data.EllipticCurve.Point (Point (..))
import           ZkFold.Symbolic.Data.FFA                (FFA, KnownFFA, unsafeFromUInt)

data ZKPassResult c = ZKPassResult
  { allocatorAddress   :: ByteString 256 c
  , allocatorSignature :: ByteString 520 c
  , publicFields       :: ByteString 1024 c
  , publicFieldsHash   :: ByteString 256 c
  , taskId             :: ByteString 256 c
  , uHash              :: ByteString 256 c
  , validatorAddress   :: ByteString 256 c
  , validatorSignature :: ByteString 520 c
  , publicKey          :: ByteString 512 c
  }

-- | Convert a 256-bit ByteString to an FFA element
fromByteString256 :: forall p ctx .
    ( S.Symbolic ctx
    , KnownFFA p 'Auto ctx
    , KnownNat (GetRegisterSize (S.BaseField ctx) 256 'Auto)
    )
    => ByteString 256 ctx
    -> FFA p 'Auto ctx
fromByteString256 = unsafeFromUInt @256 . from

hashFunction :: forall c .
    ( Keccak "Keccak256" c 1024
    )
    => ByteString 1024 c
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
     , Keccak "Keccak256" ctx 1024
     )
    => ByteString 256 ctx
    -> ByteString 256 ctx
    -> ByteString 256 ctx
    -> ByteString 520 ctx
    -> ByteString 512 ctx
    -> Bool ctx
verifyAllocatorSignature taskId validatorAddress allocatorAddress allocatorSignature publicKey = verifyVerdict
    where
        params :: ByteString 1024 ctx
        params = resize (concat $ V.unsafeToVector [taskId, allocatorAddress, validatorAddress] :: ByteString 768 ctx)

        (r, s, _) = extractSignature @p allocatorSignature

        (xVec, yVec) = splitAt (toWords publicKey) :: (Vector 1 (ByteString 256 ctx), Vector 1 (ByteString 256 ctx))

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
    -> ByteString 256 ctx
    -> ByteString 520 ctx
    -> ByteString 512 ctx
    -> Bool ctx
verifyValidatorSignature taskId uHash publicFieldsHash validatorAddress validatorSignature publicKey = verifyVerdict
    where
        params :: ByteString 1024 ctx
        params = concat $ V.unsafeToVector [taskId, validatorAddress, uHash, publicFieldsHash]

        (r, s, _) = extractSignature @p validatorSignature

        (xVec, yVec) = splitAt (toWords publicKey) :: (Vector 1 (ByteString 256 ctx), Vector 1 (ByteString 256 ctx))

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

zkPassSymbolicVerifier :: forall n p q curve ctx.
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
    => ZKPassResult ctx
    -> Bool ctx
zkPassSymbolicVerifier (ZKPassResult
    allocatorAddress
    allocatorSignature
    publicFields
    publicFieldsHash
    taskId
    uHash
    validatorAddress
    validatorSignature
    publicKey
    ) =
    let
        conditionAllocatorSignatureCorrect = verifyAllocatorSignature @n @p @q @curve
            taskId validatorAddress allocatorAddress allocatorSignature publicKey

        conditionHashEquality = hashFunction publicFields == publicFieldsHash

        conditionValidatorSignatureCorrect = verifyValidatorSignature @n @p @q @curve
            taskId uHash publicFieldsHash validatorAddress validatorSignature publicKey

    in conditionAllocatorSignatureCorrect && conditionHashEquality && conditionValidatorSignatureCorrect
