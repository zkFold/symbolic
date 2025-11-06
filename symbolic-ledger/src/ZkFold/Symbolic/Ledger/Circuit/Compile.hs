{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Symbolic.Ledger.Circuit.Compile (
  ledgerCircuit,
  ledgerSetup,
  ledgerProof,
  ZKF (..),
  ByteStringFromHex (..),
  ZKSetupBytes (..),
  ZKProofBytes (..),
  mkSetup,
  mkProof,
) where

import Data.Type.Equality (type (~))
import Data.Word (Word8)
import ZkFold.Protocol.Plonkup.Proof
import ZkFold.Protocol.Plonkup.Relation (PlonkupRelation (..))
import GHC.Generics (Generic, Generic1, Par1, U1 (..), (:*:) (..))
import GHC.TypeNats (type (+), type (^), KnownNat)
import ZkFold.Algebra.Class
import ZkFold.Protocol.Plonkup.Verifier.Commitments
import ZkFold.Protocol.Plonkup.Verifier.Setup
import ZkFold.Algebra.EllipticCurve.BLS12_381 (
  BLS12_381_G1_CompressedPoint,
  BLS12_381_G1_JacobianPoint,
  BLS12_381_G2_JacobianPoint,
 )
import ZkFold.Algebra.EllipticCurve.Jubjub (Fq)
import ZkFold.Algebra.Number qualified as Number
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.ArithmeticCircuit
import ZkFold.FFI.Rust.Plonkup (rustPlonkupProve)
import ZkFold.Protocol.NonInteractiveProof (
  FromTranscript (..),
  ToTranscript (..),
 )
import ZkFold.Protocol.NonInteractiveProof as NP (
  NonInteractiveProof (..),
  TrustedSetup (..),
 )
import ZkFold.Protocol.Plonkup (Plonkup (..))
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))
import ZkFold.Protocol.Plonkup.Utils (getParams)
import ZkFold.Protocol.Plonkup.Witness (PlonkupWitnessInput (..))
import ZkFold.Symbolic.Class (BaseField)
import ZkFold.Symbolic.Compiler qualified as C
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hash (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.MerkleTree (MerkleTree (mHash))
import ZkFold.Symbolic.Data.Vec (Vec (..), runVec)
import ZkFold.Symbolic.Interpreter
import Prelude (($), (.), Integer, Show, fromIntegral, MonadFail (..), either, pure, show, error, (<$>))
import qualified Prelude as P

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.State
import ZkFold.Algebra.Field (Zp, fromZp)
import Data.ByteString (ByteString)
import Data.Aeson (FromJSON (..), ToJSON (..), withText, Value (String))
import Data.Text (Text)
import ZkFold.Prelude (log2ceiling)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Coerce (coerce)
import GHC.Natural (naturalToInteger, Natural)
import ZkFold.Algebra.EllipticCurve.Class (Compressible(..))
import ZkFold.Data.Binary (toByteString)
import qualified Data.ByteString.Base16 as BS16

data LedgerContractInput bi bo ud a i o t c = LedgerContractInput
  { lciPreviousState :: State bi bo ud a c
  , lciTransactionBatch :: TransactionBatch i o a t c
  , lciNewState :: State bi bo ud a c
  , lciStateWitness :: StateWitness bi bo ud a i o t c
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

type LedgerContractOutput =
  (FieldElement :*: FieldElement :*: FieldElement :*: FieldElement :*: FieldElement)
    :*: (FieldElement :*: FieldElement :*: FieldElement :*: FieldElement :*: FieldElement)
    :*: Bool

ledgerContract
  :: forall bi bo ud a i o t c
   . SignatureState bi bo ud a c
  => SignatureTransactionBatch ud i o a t c
  => LedgerContractInput bi bo ud a i o t c -> LedgerContractOutput c
ledgerContract LedgerContractInput {..} =
  ( sPreviousStateHash lciPreviousState
      :*: (mHash . sUTxO $ lciPreviousState)
      :*: sLength lciPreviousState
      :*: (hHash . sBridgeIn $ lciPreviousState)
      :*: (hHash . sBridgeOut $ lciPreviousState)
  )
    :*: ( sPreviousStateHash lciNewState
            :*: (mHash . sUTxO $ lciNewState)
            :*: sLength lciNewState
            :*: (hHash . sBridgeIn $ lciNewState)
            :*: (hHash . sBridgeOut $ lciNewState)
        )
    :*: validateStateUpdate lciPreviousState lciTransactionBatch lciNewState lciStateWitness

-- TODO: Is this circuit gate count enough?
type LedgerCircuitGates = 2 ^ 18

type LedgerContractInputLayout bi bo ud a i o t =
  Layout
    (LedgerContractInput bi bo ud a i o t :*: U1)
    (Order Fq)

type LedgerContractInputPayload bi bo ud a i o t =
  Payload
    (LedgerContractInput bi bo ud a i o t :*: U1)
    (Order Fq)

type LedgerContractCompiledInput bi bo ud a i o t =
  LedgerContractInputPayload bi bo ud a i o t :*: LedgerContractInputLayout bi bo ud a i o t

type LedgerContractOutputLayout =
  ( (Par1 :*: Par1 :*: Par1 :*: Par1 :*: Par1)
      :*: (Par1 :*: Par1 :*: Par1 :*: Par1 :*: Par1)
      :*: Par1
  )

type LedgerCircuit bi bo ud a i o t =
  ArithmeticCircuit Fq (LedgerContractCompiledInput bi bo ud a i o t) LedgerContractOutputLayout

ledgerCircuit
  :: forall bi bo ud a i o t c
   . SignatureState bi bo ud a c
  => SignatureTransactionBatch ud i o a t c
  => -- Since we are hardcoding @Fq@ at some places in this file, it is important that it is the same as the base field of the context.
  Fq ~ BaseField c
  => LedgerCircuit bi bo ud a i o t
ledgerCircuit = runVec $ C.compile @Fq ledgerContract

type PlonkupTs i n t =
  Plonkup i LedgerContractOutputLayout n BLS12_381_G1_JacobianPoint BLS12_381_G2_JacobianPoint t (PolyVec Fq)

type TranscriptConstraints ts =
  ( ToTranscript ts Word8
  , ToTranscript ts Fq
  , ToTranscript ts BLS12_381_G1_CompressedPoint
  , FromTranscript ts Fq
  )

ledgerSetup
  :: forall tc bi bo ud a i o t c
   . TranscriptConstraints tc
  => SignatureState bi bo ud a c
  => SignatureTransactionBatch ud i o a t c
  => TrustedSetup (LedgerCircuitGates + 6)
  -> LedgerCircuit bi bo ud a i o t
  -> SetupVerify (PlonkupTs (LedgerContractCompiledInput bi bo ud a i o t) LedgerCircuitGates tc)
ledgerSetup TrustedSetup {..} ac = setupV
 where
  (omega, k1, k2) = getParams (Number.value @LedgerCircuitGates)
  plonkup = Plonkup omega k1 k2 ac g2_1 g1s
  setupV = setupVerify @(PlonkupTs (LedgerContractCompiledInput bi bo ud a i o t) LedgerCircuitGates tc) plonkup

ledgerProof
  :: forall tc bi bo ud a i o t c
   . (TranscriptConstraints tc, c ~ Interpreter Fq)
  => SignatureState bi bo ud a c
  => SignatureTransactionBatch ud i o a t c
  => TrustedSetup (LedgerCircuitGates + 6)
  -> PlonkupProverSecret BLS12_381_G1_JacobianPoint
  -> LedgerCircuit bi bo ud a i o t
  -> LedgerContractInput bi bo ud a i o t c
  -> Proof (PlonkupTs (LedgerContractCompiledInput bi bo ud a i o t) LedgerCircuitGates tc)
ledgerProof TrustedSetup {..} ps ac input = proof
 where
  witnessInputs :: (Layout (LedgerContractInput bi bo ud a i o t) (Order Fq)) Fq
  witnessInputs = runInterpreter $ arithmetize input

  paddedWitnessInputs :: LedgerContractCompiledInput bi bo ud a i o t Fq
  paddedWitnessInputs = (payload input :*: U1) :*: (witnessInputs :*: U1)

  (omega, k1, k2) = getParams (Number.value @LedgerCircuitGates)
  plonkup = Plonkup omega k1 k2 ac g2_1 g1s :: PlonkupTs (LedgerContractCompiledInput bi bo ud a i o t) LedgerCircuitGates tc
  setupP = setupProve @(PlonkupTs (LedgerContractCompiledInput bi bo ud a i o t) LedgerCircuitGates tc) plonkup
  witness =
    ( PlonkupWitnessInput @(LedgerContractCompiledInput bi bo ud a i o t) @BLS12_381_G1_JacobianPoint paddedWitnessInputs
    , ps
    )
  (proof, _) = rustPlonkupProve setupP witness

------------------------------------------------
-- Copy-pasted from SmartWallet.hs
------------------------------------------------

convertZp :: Zp p -> Integer
convertZp = naturalToInteger . fromZp

convertG1 :: BLS12_381_G1_JacobianPoint -> ByteString
convertG1 = toByteString . compress

convertG2 :: BLS12_381_G2_JacobianPoint -> ByteString
convertG2 = toByteString . compress

data ZKSetupBytes = ZKSetupBytes
  { n :: Integer
  , nPrv :: Integer
  , pow :: Integer
  , omega_int :: Integer
  , omegaNPrv_int :: Integer
  , k1_int :: Integer
  , k2_int :: Integer
  , h1_bytes :: ByteString
  , cmQm_bytes :: ByteString
  , cmQl_bytes :: ByteString
  , cmQr_bytes :: ByteString
  , cmQo_bytes :: ByteString
  , cmQc_bytes :: ByteString
  , cmQk_bytes :: ByteString
  , cmS1_bytes :: ByteString
  , cmS2_bytes :: ByteString
  , cmS3_bytes :: ByteString
  , cmT1_bytes :: ByteString
  , cmT2_bytes :: ByteString
  , cmT3_bytes :: ByteString
  }
  deriving stock (Generic, Show)


-- | Field element.
newtype ZKF = ZKF Integer
  deriving stock (P.Eq, Generic, P.Ord, Show)
  deriving newtype (FromJSON, ToJSON)

-- | 'ByteString' whose on wire representation is given in hexadecimal encoding.
newtype ByteStringFromHex = ByteStringFromHex ByteString
  deriving stock Generic
  deriving newtype (P.Eq, P.Ord)

byteStringFromHexToHex :: ByteStringFromHex -> Text
byteStringFromHexToHex = decodeUtf8 . BS16.encode . coerce

instance Show ByteStringFromHex where
  showsPrec d bs =
    P.showParen (d P.> 10) $
      P.showString "ByteStringFromHex "
        . P.showsPrec 11 (byteStringFromHexToHex bs)

instance FromJSON ByteStringFromHex where
  parseJSON = withText "ByteStringFromHex" $ \t ->
    either (fail . show) (pure . ByteStringFromHex) $ BS16.decode (encodeUtf8 t)

instance ToJSON ByteStringFromHex where
  toJSON = String . byteStringFromHexToHex

-- | ZK proof bytes, assuming hex encoding for relevant bytes.
data ZKProofBytes = ZKProofBytes
  { cmA_bytes :: !ByteStringFromHex
  , cmB_bytes :: !ByteStringFromHex
  , cmC_bytes :: !ByteStringFromHex
  , cmF_bytes :: !ByteStringFromHex
  , cmH1_bytes :: !ByteStringFromHex
  , cmH2_bytes :: !ByteStringFromHex
  , cmZ1_bytes :: !ByteStringFromHex
  , cmZ2_bytes :: !ByteStringFromHex
  , cmQlow_bytes :: !ByteStringFromHex
  , cmQmid_bytes :: !ByteStringFromHex
  , cmQhigh_bytes :: !ByteStringFromHex
  , proof1_bytes :: !ByteStringFromHex
  , proof2_bytes :: !ByteStringFromHex
  , a_xi_int :: !Integer
  , b_xi_int :: !Integer
  , c_xi_int :: !Integer
  , s1_xi_int :: !Integer
  , s2_xi_int :: !Integer
  , f_xi_int :: !Integer
  , t_xi_int :: !Integer
  , t_xi'_int :: !Integer
  , z1_xi'_int :: !Integer
  , z2_xi'_int :: !Integer
  , h1_xi'_int :: !Integer
  , h2_xi_int :: !Integer
  , l_xi :: ![ZKF]
  , l1_xi :: !ZKF
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


mkSetup :: forall i n. KnownNat n => SetupVerify (PlonkupTs i n ByteString) -> ZKSetupBytes
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

mkProof :: forall i (n :: Natural). Proof (PlonkupTs i n ByteString) -> ZKProofBytes
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
-- End of copy-pasted code from SmartWallet.hs
------------------------------------------------