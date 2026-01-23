{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Symbolic.Cardano.Contracts.ZkPass where

import ZkFold.ArithmeticCircuit.Op (Sort (..))
import Data.ByteString qualified as Haskell (ByteString)
import ZkFold.Algebra.Number qualified as Number
import ZkFold.FFI.Rust.Plonkup (rustPlonkupProve)
import           GHC.Generics                            ((:*:) (..), U1 (..), Par1 (..), Generic, Generic1)
import           GHC.TypeLits                            (KnownNat, type (^), type (+))
import           Prelude                                 hiding (Bool, Eq (..), concat, head, length, splitAt, (!!),
                                                          (&&), (*), (+))

import           ZkFold.Algebra.Class                    (zero, Order, (+))
import           ZkFold.Algebra.EllipticCurve.Class      hiding (Point)
import ZkFold.Protocol.NonInteractiveProof (
  FromTranscript (..),
  ToTranscript (..),
 )
import ZkFold.Protocol.NonInteractiveProof as NP (
  NonInteractiveProof (..),
  TrustedSetup (..),
 )
import ZkFold.ArithmeticCircuit.Node qualified as C
import ZkFold.Protocol.Plonkup (Plonkup (..))
import ZkFold.Protocol.Plonkup.Utils (getParams)
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
import           ZkFold.Symbolic.Data.FieldElement       (FieldElement (..))
import           ZkFold.Symbolic.Data.UInt               (UInt, toNative)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar, BLS12_381_G1_JacobianPoint, BLS12_381_G2_JacobianPoint, BLS12_381_G1_CompressedPoint)
import ZkFold.Algebra.Field (Zp, fromZp)
import ZkFold.Symbolic.Data.Class (SymbolicData(..))
import ZkFold.ArithmeticCircuit (ArithmeticCircuit)
import ZkFold.Symbolic.Compat (CompatContext)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import Data.Word (Word8)
import ZkFold.Symbolic.Interpreter (Interpreter (..))
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret)
import ZkFold.Protocol.Plonkup.Witness (PlonkupWitnessInput(..))
import ZkFold.Protocol.Plonkup.Verifier.Setup
import ZkFold.Protocol.Plonkup.Verifier (PlonkupCircuitCommitments(..))
import ZkFold.Protocol.Plonkup.OffChain.Cardano (ZKSetupBytes(..), ZKProofBytes (..), ByteStringFromHex (..), ZKF (..))
import ZkFold.Protocol.Plonkup.Proof (PlonkupProof(..))
import GHC.Natural (naturalToInteger)
import ZkFold.Protocol.Plonkup.Relation (PlonkupRelation(..))
import ZkFold.Prelude (log2ceiling)
import ZkFold.Data.Binary (toByteString)
import Data.Aeson (FromJSON (..), eitherDecode)
import Data.ByteString.Lazy qualified as LBS

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

type PlonkupTs i n t =
  Plonkup
    i
    ZkPassContractOutputLayout
    n
    BLS12_381_G1_JacobianPoint
    BLS12_381_G2_JacobianPoint
    t
    (PolyVec ZkPassBF)

type TranscriptConstraints ts =
  ( ToTranscript ts Word8
  , ToTranscript ts ZkPassBF
  , ToTranscript ts BLS12_381_G1_CompressedPoint
  , FromTranscript ts ZkPassBF
  )

type ZkPassCircuitGates = 2 ^ 18

zksPassSetup
  :: forall tc
   . TranscriptConstraints tc
  => TrustedSetup (ZkPassCircuitGates + 6)
  -> ZkPassCircuit
  -> SetupVerify (PlonkupTs ZkPassContractCompiledInput ZkPassCircuitGates tc)
zksPassSetup TrustedSetup {..} circuit = setupV
 where
  (omega, k1, k2) = getParams (Number.value @ZkPassCircuitGates)
  plonkup = Plonkup omega k1 k2 circuit g2_1 g1s
  setupV = setupVerify @(PlonkupTs ZkPassContractCompiledInput ZkPassCircuitGates tc) plonkup

zksPassProof
  :: forall tc c
   . (TranscriptConstraints tc, c ~ Interpreter ZkPassBF)
  => TrustedSetup (ZkPassCircuitGates + 6)
  -> PlonkupProverSecret BLS12_381_G1_JacobianPoint
  -> ZkPassCircuit
  -> ZkPassContractInput c
  -> Proof (PlonkupTs ZkPassContractCompiledInput ZkPassCircuitGates tc)
zksPassProof TrustedSetup {..} ps circuit input = proof
 where
  witnessInputs :: (Layout ZkPassContractInput (Order ZkPassBF)) ZkPassBF
  witnessInputs = runInterpreter $ arithmetize input

  paddedWitnessInputs :: ZkPassContractCompiledInput ZkPassBF
  paddedWitnessInputs = (witnessInputs :*: U1) :*: (payload input :*: U1)

  (omega, k1, k2) = getParams (Number.value @ZkPassCircuitGates)
  plonkup =
    Plonkup omega k1 k2 circuit g2_1 g1s
      :: PlonkupTs ZkPassContractCompiledInput ZkPassCircuitGates tc
  setupP = setupProve @(PlonkupTs ZkPassContractCompiledInput ZkPassCircuitGates tc) plonkup
  witness =
    ( PlonkupWitnessInput @ZkPassContractCompiledInput @BLS12_381_G1_JacobianPoint paddedWitnessInputs
    , ps
    )
  (proof, _) = rustPlonkupProve setupP witness

deriving anyclass instance FromJSON (ZKPassResult (Interpreter ZkPassBF))

parseZKPassResult :: String -> Either String (ZKPassResult (Interpreter ZkPassBF))
parseZKPassResult = eitherDecode . LBS.fromStrict . fromString
  where
    fromString = LBS.toStrict . LBS.pack . fmap (fromIntegral . fromEnum)

zkPassVerifyInput
  :: forall n p q curve
   . Signature n p q curve (Interpreter ZkPassBF)
  => ZKPassResult (Interpreter ZkPassBF)
  -> ZkPassBF
zkPassVerifyInput input = runInterpreter $ fromFieldElement $ zkPassSymbolicVerifier @n @p @q @curve input
  where
    fromFieldElement (FieldElement (Interpreter (Par1 x))) = Interpreter x

------------------------------------------------
-- Copy-pasted from ZkFold.Symbolic.Examples.SmartWallet
------------------------------------------------

convertZp :: Zp p -> Integer
convertZp = naturalToInteger . fromZp

convertG1 :: BLS12_381_G1_JacobianPoint -> Haskell.ByteString
convertG1 = toByteString . compress

convertG2 :: BLS12_381_G2_JacobianPoint -> Haskell.ByteString
convertG2 = toByteString . compress

mkSetup :: forall i n . KnownNat n => SetupVerify (PlonkupTs i n Haskell.ByteString) -> ZKSetupBytes
mkSetup PlonkupVerifierSetup {..} =
  let PlonkupCircuitCommitments {..} = commitments
   in ZKSetupBytes
        { n = fromIntegral (Number.value @n)
        , nPrv = fromIntegral $ prvNum relation
        , pow = log2ceiling (Number.value @n)
        , omega_int = convertZp omega
        , omegaNPrv_int = convertZp (omega ^ (prvNum relation + 1))
        , k1_int = convertZp k1
        , k2_int = convertZp k2
        , h1_bytes = convertG2 h1
        , cmQm_bytes = convertG1 cmQm
        , cmQl_bytes = convertG1 cmQl
        , cmQr_bytes = convertG1 cmQr
        , cmQo_bytes = convertG1 cmQo
        , cmQc_bytes = convertG1 cmQc
        , cmQk_bytes = convertG1 cmQk
        , cmS1_bytes = convertG1 cmS1
        , cmS2_bytes = convertG1 cmS2
        , cmS3_bytes = convertG1 cmS3
        , cmT1_bytes = convertG1 cmT1
        , cmT2_bytes = convertG1 cmT2
        , cmT3_bytes = convertG1 cmT3
        }

mkProof :: forall i n . Proof (PlonkupTs i n Haskell.ByteString) -> ZKProofBytes
mkProof PlonkupProof {..} =
  case l_xi of
    [] -> error "mkProof: empty inputs"
    xs ->
      ZKProofBytes
        { cmA_bytes = ByteStringFromHex $ convertG1 cmA
        , cmB_bytes = ByteStringFromHex $ convertG1 cmB
        , cmC_bytes = ByteStringFromHex $ convertG1 cmC
        , cmF_bytes = ByteStringFromHex $ convertG1 cmF
        , cmH1_bytes = ByteStringFromHex $ convertG1 cmH1
        , cmH2_bytes = ByteStringFromHex $ convertG1 cmH2
        , cmZ1_bytes = ByteStringFromHex $ convertG1 cmZ1
        , cmZ2_bytes = ByteStringFromHex $ convertG1 cmZ2
        , cmQlow_bytes = ByteStringFromHex $ convertG1 cmQlow
        , cmQmid_bytes = ByteStringFromHex $ convertG1 cmQmid
        , cmQhigh_bytes = ByteStringFromHex $ convertG1 cmQhigh
        , proof1_bytes = ByteStringFromHex $ convertG1 proof1
        , proof2_bytes = ByteStringFromHex $ convertG1 proof2
        , a_xi_int = convertZp a_xi
        , b_xi_int = convertZp b_xi
        , c_xi_int = convertZp c_xi
        , s1_xi_int = convertZp s1_xi
        , s2_xi_int = convertZp s2_xi
        , f_xi_int = convertZp f_xi
        , t_xi_int = convertZp t_xi
        , t_xi'_int = convertZp t_xi'
        , z1_xi'_int = convertZp z1_xi'
        , z2_xi'_int = convertZp z2_xi'
        , h1_xi'_int = convertZp h1_xi'
        , h2_xi_int = convertZp h2_xi
        , l_xi = ZKF . convertZp <$> xs
        , l1_xi = ZKF $ convertZp l1_xi
        }

------------------------------------------------
-- End of copy-pasted code from ZkFold.Symbolic.Examples.SmartWallet
------------------------------------------------
