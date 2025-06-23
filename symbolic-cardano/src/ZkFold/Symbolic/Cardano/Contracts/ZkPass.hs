{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Cardano.Contracts.ZkPass where

-- import           Data.Type.Equality
-- import           GHC.TypeLits                            (KnownNat, Log2)
-- import qualified GHC.TypeNats
-- import           Prelude                                 hiding (Bool, Eq (..), concat, head, length, splitAt, (!!),
--                                                           (&&), (*), (+))

-- import           ZkFold.Base.Algebra.Basic.Class
-- import           ZkFold.Base.Algebra.EllipticCurve.Class
-- import qualified ZkFold.Base.Data.Vector                 as V
-- import           ZkFold.Base.Data.Vector                 hiding (concat)
-- import           ZkFold.Symbolic.Algorithms.ECDSA.ECDSA  (ecdsaVerify)
-- import           ZkFold.Symbolic.Algorithms.Hash.SHA2    (SHA2N, sha2)
-- import qualified ZkFold.Symbolic.Class                   as S
-- import           ZkFold.Symbolic.Class                   (Symbolic)
-- import           ZkFold.Symbolic.Data.Bool
-- import           ZkFold.Symbolic.Data.ByteString         (ByteString, concat, toWords)
-- import           ZkFold.Symbolic.Data.Combinators        (Iso (..), NumberOfRegisters, RegisterSize (..), resize)
-- import           ZkFold.Symbolic.Data.Eq
-- import           ZkFold.Symbolic.Data.FieldElement       (FieldElement)
-- import           ZkFold.Symbolic.Data.UInt               (UInt)

-- data ZKPassResult c = ZKPassResult
--   { allocatorAddress   :: ByteString 256 c
--   , allocatorSignature :: ByteString 520 c
--   , publicFields       :: ByteString 1024 c
--   , publicFieldsHash   :: ByteString 256 c
--   , taskId             :: ByteString 256 c
--   , uHash              :: ByteString 256 c
--   , validatorAddress   :: ByteString 256 c
--   , validatorSignature :: ByteString 520 c
--   , publicKey          :: ByteString 512 c
--   }

-- type Hash c = UInt 256 Auto c

-- -- TODO: change to keccak256
-- hashFunction :: forall c . (
--     SHA2N "SHA512/256" c
--     )
--     => ByteString 1024 c
--     -> ByteString 256 c
-- hashFunction = sha2 @"SHA512/256"

-- verifyAllocatorSignature :: forall point n context curve baseField.
--      ( S.Symbolic context
--      , KnownNat n
--      , baseField ~ UInt 256 'Auto context
--      , ScalarFieldOf point ~ FieldElement context
--      , point ~ Weierstrass curve (Point baseField)
--      , CyclicGroup point
--      , SemiEuclidean (UInt 256 'Auto context)
--      , KnownNat (NumberOfRegisters (S.BaseField context) 256 'Auto)
--      , Log2 (Order (S.BaseField context) GHC.TypeNats.- 1) ~ 255
--      )
--     => ByteString 256 context
--     -> ByteString 256 context
--     -> ByteString 256 context
--     -> ByteString 520 context
--     -> ByteString 512 context
--     -> Bool context
-- verifyAllocatorSignature taskId validatorAddress allocatorAddress allocatorSignature publicKey = verifyVerdict
--     where
--         params :: ByteString 1024 context
--         params = resize (concat $ V.unsafeToVector [taskId, validatorAddress, allocatorAddress] :: ByteString 768 context)

--         encodedParams :: ByteString 256 context
--         encodedParams = hashFunction params

--         (r, s, _) = extractSignature allocatorSignature

--         (x, y) = splitAt (toWords publicKey) :: (Vector 1 (ByteString 256 context), Vector 1 (ByteString 256 context))

--         verifyVerdict :: Bool context
--         verifyVerdict = ecdsaVerify @point @n @context @curve @baseField (pointXY (from $ item x) (from $ item y)) encodedParams (r, s)

-- verifyValidatorSignature :: forall point n context curve baseField.
--      ( S.Symbolic context
--      , KnownNat n
--      , baseField ~ UInt 256 'Auto context
--      , ScalarFieldOf point ~ FieldElement context
--      , point ~ Weierstrass curve (Point baseField)
--      , CyclicGroup point
--      , SemiEuclidean (UInt 256 'Auto context)
--      , KnownNat (NumberOfRegisters (S.BaseField context) 256 'Auto)
--      , Log2 (Order (S.BaseField context) GHC.TypeNats.- 1) ~ 255
--      )
--     => ByteString 256 context
--     -> ByteString 256 context
--     -> ByteString 256 context
--     -> ByteString 256 context
--     -> ByteString 520 context
--     -> ByteString 512 context
--     -> Bool context
-- verifyValidatorSignature taskId uHash publicFieldsHash validatorAddress validatorSignature publicKey = verifyVerdict
--     where
--         params :: ByteString 1024 context
--         params = concat $ V.unsafeToVector [taskId, uHash, publicFieldsHash, validatorAddress]

--         encodedParams :: ByteString 256 context
--         encodedParams = hashFunction params

--         (r, s, _) = extractSignature validatorSignature

--         (x, y) = splitAt (toWords publicKey) :: (Vector 1 (ByteString 256 context), Vector 1 (ByteString 256 context))

--         verifyVerdict :: Bool context
--         verifyVerdict = ecdsaVerify @point @n @context @curve @baseField (pointXY (from $ item x) (from $ item y)) encodedParams (r, s)

-- extractSignature :: forall context . (Symbolic context)
--     => ByteString 520 context
--     -> (Hash context,  Hash context, ByteString 8 context)
-- extractSignature sign = (from $ concat r', from $ concat s', item v')
--     where
--         r' :: Vector 32 (ByteString 8 context)

--         bytes = toWords sign

--         (r', l') = splitAt bytes

--         (s', v') = splitAt l'

-- zkPassSymbolicVerifier :: forall point n context curve baseField.
--      ( S.Symbolic context
--      , KnownNat n
--      , baseField ~ UInt 256 'Auto context
--      , ScalarFieldOf point ~ FieldElement context
--      , point ~ Weierstrass curve (Point baseField)
--      , CyclicGroup point
--      , SemiEuclidean (UInt 256 'Auto context)
--      , KnownNat (NumberOfRegisters (S.BaseField context) 256 'Auto)
--      , Log2 (Order (S.BaseField context) GHC.TypeNats.- 1) ~ 255
--      )
--     =>ZKPassResult context
--     -> Bool context
-- zkPassSymbolicVerifier (ZKPassResult
--     allocatorAddress
--     allocatorSignature
--     publicFields
--     publicFieldsHash
--     taskId
--     uHash
--     validatorAddress
--     validatorSignature
--     publicKey
--     ) =
--     let
--         conditionAllocatorSignatureCorrect = verifyAllocatorSignature @point @n @context @curve @baseField
--             taskId validatorAddress allocatorAddress allocatorSignature publicKey

--         conditionHashEquality = hashFunction publicFields == publicFieldsHash

--         conditionValidatorSignatureCorrect = verifyValidatorSignature @point @n @context @curve @baseField
--             taskId uHash publicFieldsHash validatorAddress validatorSignature publicKey

--     in conditionAllocatorSignatureCorrect && conditionHashEquality && conditionValidatorSignatureCorrect
