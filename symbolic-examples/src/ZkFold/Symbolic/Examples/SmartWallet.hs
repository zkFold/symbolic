{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Examples.SmartWallet
    ( expModContract
    , expModCircuit
    , expModSetup
    , expModProof
    , ExpModCircuitGates
    , ExpModProofInput (..)
    , PlonkupTs

    , expModSetupMock
    , expModProofMock
    , ExpModCircuitGatesMock

    , ZKSetupBytes (..)
    , ZKProofBytes (..)
    , ZKF (..)
    , ByteStringFromHex (..)
    , mkProof
    , mkSetup
    ) where

import           Data.Aeson                                   (withText)
import qualified Data.Aeson                                   as Aeson
import           Data.ByteString                              (ByteString)
import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Base16                       as BS16
import           Data.Coerce                                  (coerce)
import           Data.Foldable                                (foldrM)
import           Data.Proxy
import           Data.Text                                    (Text)
import           Data.Text.Encoding                           (decodeUtf8, encodeUtf8)
import           Data.Word                                    (Word8)
import           Deriving.Aeson
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           GHC.Generics                                 (Par1 (..), U1 (..), type (:*:) (..))
import           GHC.Natural                                  (naturalToInteger)
import           Prelude                                      hiding (Fractional (..), Num (..), length, (^))
import qualified Prelude                                      as P

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.BLS12_381       (BLS12_381_G1_CompressedPoint, BLS12_381_G1_Point,
                                                               BLS12_381_G2_Point, Fr)
import           ZkFold.Algebra.EllipticCurve.Class           (compress)
import           ZkFold.Algebra.Field                         (Zp, fromZp, toZp)
import qualified ZkFold.Algebra.Number                        as Number
import           ZkFold.Algebra.Number                        (KnownNat, Natural, type (^))
import           ZkFold.Algebra.Polynomial.Univariate         (PolyVec)
import           ZkFold.Data.ByteString                       (toByteString)
import qualified ZkFold.Data.Vector                           as V
import           ZkFold.Data.Vector                           (Vector)
import           ZkFold.Prelude                               (log2ceiling)
import           ZkFold.Protocol.NonInteractiveProof          as NP (FromTranscript (..), NonInteractiveProof (..),
                                                                     ToTranscript (..))
import           ZkFold.Protocol.Plonkup                      (Plonkup (..))
import           ZkFold.Protocol.Plonkup.Proof
import           ZkFold.Protocol.Plonkup.Prover.Secret        (PlonkupProverSecret (..))
import           ZkFold.Protocol.Plonkup.Relation             (PlonkupRelation (..))
import           ZkFold.Protocol.Plonkup.Utils                (getParams, getSecretParams)
import           ZkFold.Protocol.Plonkup.Verifier.Commitments
import           ZkFold.Protocol.Plonkup.Verifier.Setup
import           ZkFold.Protocol.Plonkup.Witness              (PlonkupWitnessInput (..))
import qualified ZkFold.Symbolic.Algorithm.RSA                as RSA
import           ZkFold.Symbolic.Class                        (Symbolic (..))
import qualified ZkFold.Symbolic.Compiler                     as C
import qualified ZkFold.Symbolic.Compiler.ArithmeticCircuit   as AC
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit   (ArithmeticCircuit (..))
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.FieldElement
import           ZkFold.Symbolic.Data.Input
import           ZkFold.Symbolic.Data.UInt                    (OrdWord, UInt (..), exp65537Mod)
import           ZkFold.Symbolic.Interpreter
import           ZkFold.Symbolic.MonadCircuit                 (newAssigned)

-- Copypaste from zkfold-cardano but these types do not depend on PlutusTx
--
convertZp :: Zp p -> Integer
convertZp = naturalToInteger . fromZp

convertG1 :: BLS12_381_G1_Point -> ByteString
convertG1 = toByteString . compress

convertG2 :: BLS12_381_G2_Point -> ByteString
convertG2 = toByteString . compress

data ZKSetupBytes = ZKSetupBytes {
    n             :: Integer
  , nPrv          :: Integer
  , pow           :: Integer
  , omega_int     :: Integer
  , omegaNPrv_int :: Integer
  , k1_int        :: Integer
  , k2_int        :: Integer
  , h1_bytes      :: ByteString
  , cmQm_bytes    :: ByteString
  , cmQl_bytes    :: ByteString
  , cmQr_bytes    :: ByteString
  , cmQo_bytes    :: ByteString
  , cmQc_bytes    :: ByteString
  , cmQk_bytes    :: ByteString
  , cmS1_bytes    :: ByteString
  , cmS2_bytes    :: ByteString
  , cmS3_bytes    :: ByteString
  , cmT1_bytes    :: ByteString
  , cmT2_bytes    :: ByteString
  , cmT3_bytes    :: ByteString
} deriving stock (Show, Generic)

mkSetup :: forall i n. KnownNat n => SetupVerify (PlonkupTs i n ByteString) -> ZKSetupBytes
mkSetup PlonkupVerifierSetup {..} =
  let PlonkupCircuitCommitments {..} = commitments
  in ZKSetupBytes
    { n             = fromIntegral (Number.value @n)
    , nPrv          = fromIntegral $ prvNum relation
    , pow           = log2ceiling (Number.value @n)
    , omega_int     = convertZp omega
    , omegaNPrv_int = convertZp (omega ^ (prvNum relation + 1))
    , k1_int        = convertZp k1
    , k2_int        = convertZp k2
    , h1_bytes      = convertG2 h1
    , cmQm_bytes    = convertG1 cmQm
    , cmQl_bytes    = convertG1 cmQl
    , cmQr_bytes    = convertG1 cmQr
    , cmQo_bytes    = convertG1 cmQo
    , cmQc_bytes    = convertG1 cmQc
    , cmQk_bytes    = convertG1 cmQk
    , cmS1_bytes    = convertG1 cmS1
    , cmS2_bytes    = convertG1 cmS2
    , cmS3_bytes    = convertG1 cmS3
    , cmT1_bytes    = convertG1 cmT1
    , cmT2_bytes    = convertG1 cmT2
    , cmT3_bytes    = convertG1 cmT3
    }

-- | Field element.
newtype ZKF = ZKF Integer
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | 'ByteString' whose on wire representation is given in hexadecimal encoding.
newtype ByteStringFromHex = ByteStringFromHex ByteString
  deriving stock (Generic)
  deriving newtype (Eq, Ord)

byteStringFromHexToHex :: ByteStringFromHex -> Text
byteStringFromHexToHex = decodeUtf8 . BS16.encode . coerce

instance Show ByteStringFromHex where
  showsPrec d bs =
    showParen (d > 10) $
      showString "ByteStringFromHex "
        . showsPrec 11 (byteStringFromHexToHex bs)

instance FromJSON ByteStringFromHex where
  parseJSON = withText "ByteStringFromHex" $ \t ->
    either (fail . show) (pure . ByteStringFromHex) $ BS16.decode (encodeUtf8 t)

instance ToJSON ByteStringFromHex where
  toJSON = Aeson.String . byteStringFromHexToHex

-- | ZK proof bytes, assuming hex encoding for relevant bytes.
data ZKProofBytes = ZKProofBytes
  { cmA_bytes     :: !ByteStringFromHex
  , cmB_bytes     :: !ByteStringFromHex
  , cmC_bytes     :: !ByteStringFromHex
  , cmF_bytes     :: !ByteStringFromHex
  , cmH1_bytes    :: !ByteStringFromHex
  , cmH2_bytes    :: !ByteStringFromHex
  , cmZ1_bytes    :: !ByteStringFromHex
  , cmZ2_bytes    :: !ByteStringFromHex
  , cmQlow_bytes  :: !ByteStringFromHex
  , cmQmid_bytes  :: !ByteStringFromHex
  , cmQhigh_bytes :: !ByteStringFromHex
  , proof1_bytes  :: !ByteStringFromHex
  , proof2_bytes  :: !ByteStringFromHex
  , a_xi_int      :: !Integer
  , b_xi_int      :: !Integer
  , c_xi_int      :: !Integer
  , s1_xi_int     :: !Integer
  , s2_xi_int     :: !Integer
  , f_xi_int      :: !Integer
  , t_xi_int      :: !Integer
  , t_xi'_int     :: !Integer
  , z1_xi'_int    :: !Integer
  , z2_xi'_int    :: !Integer
  , h1_xi'_int    :: !Integer
  , h2_xi_int     :: !Integer
  , l_xi          :: !ZKF
  , l1_xi         :: !ZKF
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

mkProof :: forall i (n :: Natural) . Proof (PlonkupTs i n ByteString) -> ZKProofBytes
mkProof PlonkupProof {..} =
    case l_xi of
      [] -> error "mkProof: empty inputs"
      (xi:_) -> ZKProofBytes
        { cmA_bytes     = ByteStringFromHex $ convertG1 cmA
        , cmB_bytes     = ByteStringFromHex $ convertG1 cmB
        , cmC_bytes     = ByteStringFromHex $ convertG1 cmC
        , cmF_bytes     = ByteStringFromHex $ convertG1 cmF
        , cmH1_bytes    = ByteStringFromHex $ convertG1 cmH1
        , cmH2_bytes    = ByteStringFromHex $ convertG1 cmH2
        , cmZ1_bytes    = ByteStringFromHex $ convertG1 cmZ1
        , cmZ2_bytes    = ByteStringFromHex $ convertG1 cmZ2
        , cmQlow_bytes  = ByteStringFromHex $ convertG1 cmQlow
        , cmQmid_bytes  = ByteStringFromHex $ convertG1 cmQmid
        , cmQhigh_bytes = ByteStringFromHex $ convertG1 cmQhigh
        , proof1_bytes  = ByteStringFromHex $ convertG1 proof1
        , proof2_bytes  = ByteStringFromHex $ convertG1 proof2
        , a_xi_int      = convertZp a_xi
        , b_xi_int      = convertZp b_xi
        , c_xi_int      = convertZp c_xi
        , s1_xi_int     = convertZp s1_xi
        , s2_xi_int     = convertZp s2_xi
        , f_xi_int      = convertZp f_xi
        , t_xi_int      = convertZp t_xi
        , t_xi'_int     = convertZp t_xi'
        , z1_xi'_int    = convertZp z1_xi'
        , z2_xi'_int    = convertZp z2_xi'
        , h1_xi'_int    = convertZp h1_xi'
        , h2_xi_int     = convertZp h2_xi
        , l_xi          = ZKF $ convertZp l1_xi
        , l1_xi         = ZKF $ convertZp xi
        }

type ExpModCircuitGates = 2^7

type ExpModLayout = ((Vector 1 :*: Vector 17) :*: (Vector 17 :*: Par1))
type ExpModCompiledInput = (((U1 :*: U1) :*: (U1 :*: U1)) :*: U1) :*: (ExpModLayout :*: U1)
type ExpModCircuit = ArithmeticCircuit Fr ExpModCompiledInput Par1

type PlonkupTs i n t = Plonkup i Par1 n BLS12_381_G1_Point BLS12_381_G2_Point t (PolyVec Fr)

type TranscriptConstraints ts =
    ( ToTranscript ts Word8
    , ToTranscript ts Fr
    , ToTranscript ts BLS12_381_G1_CompressedPoint
    , FromTranscript ts Fr
    )

data ExpModInput c
    = ExpModInput
        { emiPKey      :: RSA.PublicKey 2048 c
        , emiSig       :: UInt 2048 Auto c
        , emiTokenName :: FieldElement c
        } deriving Generic

deriving instance
    ( Symbolic ctx
    , KnownRegisters ctx RSA.PubExponentSize 'Auto
    , KnownRegisters ctx 2048 'Auto
    ) => SymbolicData (ExpModInput ctx)

--deriving instance
instance
    ( Symbolic ctx
    , KnownRegisters ctx RSA.PubExponentSize 'Auto
    , KnownRegisters ctx 2048 'Auto
    ) => SymbolicInput (ExpModInput ctx) where
        isValid _ = true


expModContract
    :: forall c
    .  Symbolic c
    => KnownNat (NumberOfRegisters (BaseField c) 4096 Auto)
    => KnownNat (Ceil (GetRegisterSize (BaseField c) 4096 Auto) OrdWord)
    => ExpModInput c
    -> FieldElement c
expModContract (ExpModInput RSA.PublicKey{..} sig tokenNameAsFE) = hashAsFE * tokenNameAsFE
    where
        msgHash :: UInt 2048 Auto c
        msgHash = sig -- exp65537Mod @c @2048 @RSA.PubExponentSize @2048 sig pubN

        rsize :: Natural
        rsize = registerSize @(BaseField c) @2048 @Auto

        -- UInt `mod` BLS12_381_Scalar is just weighted sum of its registers
        --
        hashAsFE :: FieldElement c
        hashAsFE = FieldElement $ fromCircuitF (let UInt regs = msgHash in regs) $ \v -> do
            z <- newAssigned (const zero)
            ans <- foldrM (\a i -> newAssigned $ \p -> scale rsize (p a) + p i) z v
            temporaryMeaninglessAns <- foldrM (\a i -> newAssigned $ \p -> p i + scale a (p z)) ans [0..63 :: Natural]
            pure $ Par1 temporaryMeaninglessAns

expModCircuit :: ExpModCircuit
expModCircuit = C.compile @Fr expModContract

expModSetup :: forall t .  TranscriptConstraints t => Fr -> ExpModCircuit -> SetupVerify (PlonkupTs ExpModCompiledInput ExpModCircuitGates t)
expModSetup x ac = setupV
    where
        (omega, k1, k2) = getParams (Number.value @ExpModCircuitGates)
        (gs, h1) = getSecretParams @ExpModCircuitGates @BLS12_381_G1_Point @BLS12_381_G2_Point x
        plonkup = Plonkup omega k1 k2 ac h1 gs
        setupV  = setupVerify @(PlonkupTs ExpModCompiledInput ExpModCircuitGates t) plonkup

data ExpModProofInput =
    ExpModProofInput
        { piPubE      :: Natural
        , piPubN      :: Natural
        , piSignature :: Natural
        , piTokenName :: Natural
        }
    deriving P.Show

expModProof
    :: forall t
    .  TranscriptConstraints t
    => Fr
    -> PlonkupProverSecret BLS12_381_G1_Point
    -> ExpModCircuit
    -> ExpModProofInput
    -> Proof (PlonkupTs ExpModCompiledInput ExpModCircuitGates t)
expModProof x ps ac ExpModProofInput{..} = proof
    where
        input :: ExpModInput (Interpreter Fr)
        input =
            ExpModInput
                (RSA.PublicKey
                    (fromConstant piPubE)
                    (fromConstant piPubN))
                (fromConstant piSignature)
                (fromConstant piTokenName)

        witnessInputs :: ExpModLayout Fr
        witnessInputs = runInterpreter $ arithmetize input Proxy

        paddedWitnessInputs :: ExpModCompiledInput Fr
        paddedWitnessInputs = (((U1 :*: U1) :*: (U1 :*: U1)) :*: U1) :*: (witnessInputs :*: U1)

        (omega, k1, k2) = getParams (Number.value @ExpModCircuitGates)
        (gs, h1) = getSecretParams @ExpModCircuitGates @BLS12_381_G1_Point @BLS12_381_G2_Point x
        plonkup = Plonkup omega k1 k2 ac h1 gs :: PlonkupTs ExpModCompiledInput ExpModCircuitGates t
        setupP  = setupProve @(PlonkupTs ExpModCompiledInput ExpModCircuitGates t) plonkup
        witness = (PlonkupWitnessInput @ExpModCompiledInput @BLS12_381_G1_Point paddedWitnessInputs, ps)
        (_, proof) = prove @(PlonkupTs ExpModCompiledInput ExpModCircuitGates t) setupP witness


-------------------------------------------------------------------------------------------------------------------
--  Mock circuit. To be replaced with the full circuit after optimisations
-------------------------------------------------------------------------------------------------------------------


type ExpModCircuitGatesMock = 2^10

identityCircuit :: ArithmeticCircuit Fr Par1 Par1
identityCircuit = AC.idCircuit

expModSetupMock :: forall t . TranscriptConstraints t => Fr -> SetupVerify (PlonkupTs Par1 ExpModCircuitGatesMock t)
expModSetupMock x = setupV
    where
        (omega, k1, k2) = getParams (Number.value @ExpModCircuitGatesMock)
        (gs, h1) = getSecretParams @ExpModCircuitGatesMock @BLS12_381_G1_Point @BLS12_381_G2_Point x
        plonkup = Plonkup omega k1 k2 identityCircuit h1 gs
        setupV  = setupVerify @(PlonkupTs Par1 ExpModCircuitGatesMock t) plonkup

expModProofMock
    :: forall t
    .  TranscriptConstraints t
    => Fr
    -> PlonkupProverSecret BLS12_381_G1_Point
    -> ExpModProofInput
    -> Proof (PlonkupTs Par1 ExpModCircuitGatesMock t)
expModProofMock x ps ExpModProofInput{..} = proof
    where
        expm :: Natural
        expm = (piSignature P.^ piPubE) `P.mod` piPubN

        hash :: Natural
        hash = expm `P.mod` (2 P.^ (256 :: Natural))

        input :: Fr
        input = toZp (fromIntegral hash) * toZp (fromIntegral piTokenName)

        witnessInputs :: Par1 Fr
        witnessInputs = Par1 input

        (omega, k1, k2) = getParams (Number.value @ExpModCircuitGatesMock)
        (gs, h1) = getSecretParams @ExpModCircuitGatesMock @BLS12_381_G1_Point @BLS12_381_G2_Point x
        plonkup = Plonkup omega k1 k2 identityCircuit h1 gs
        setupP  = setupProve @(PlonkupTs Par1 ExpModCircuitGatesMock t) plonkup
        witness = (PlonkupWitnessInput @Par1 @BLS12_381_G1_Point witnessInputs, ps)
        (_, proof) = prove @(PlonkupTs Par1 ExpModCircuitGatesMock t) setupP witness

foreign export ccall mkProofBytesWasm :: CString -> CString -> CString -> IO CString
foreign export ccall mkProofBytesMockWasm :: CString -> CString -> CString -> IO CString

mkProofBytesWasm :: CString -> CString -> CString -> IO CString
mkProofBytesWasm xPtr psPtr proofInputPtr = do
    (x, ps, proofInput) <- readPointers xPtr psPtr proofInputPtr
    let proofBytes = mkProof $ expModProof @ByteString x ps expModCircuit proofInput
    let json = fmap (CChar . fromIntegral) . BS.unpack . BS.toStrict . Aeson.encode $ proofBytes
    newArray (json <> [CChar 0])

mkProofBytesMockWasm :: CString -> CString -> CString -> IO CString
mkProofBytesMockWasm xPtr psPtr proofInputPtr = do
    (x, ps, proofInput) <- readPointers xPtr psPtr proofInputPtr
    let mockProofBytes = mkProof $ expModProofMock @ByteString x ps proofInput
    let json = fmap (CChar . fromIntegral) . BS.unpack . BS.toStrict . Aeson.encode $ mockProofBytes
    newArray (json <> [CChar 0])

readPointers :: CString -> CString -> CString -> IO (Fr, PlonkupProverSecret BLS12_381_G1_Point, ExpModProofInput)
readPointers xPtr psPtr proofInputPtr = do
    xStr <- peekCString xPtr
    let x = toZp $ read xStr

    psStr <- peekCString psPtr
    let ps = PlonkupProverSecret $ V.unsafeToVector $ (toZp . read) <$> words psStr

    [e, m, s, t] <- words <$> peekCString proofInputPtr
    let empi = ExpModProofInput (read e) (read m) (read s) (read t)

    pure (x, ps, empi)
