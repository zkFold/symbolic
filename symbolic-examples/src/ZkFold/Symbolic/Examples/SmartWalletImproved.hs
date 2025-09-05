{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Examples.SmartWalletImproved (
  expModContract,
  expModCircuit,
  expModSetup,
  expModProof,
  ExpModProofInput (..),
  PlonkupTs,
  ExpModCircuitGates,
  ZKSetupBytes (..),
  ZKProofBytes (..),
  ZKF (..),
  ByteStringFromHex (..),
  mkProof,
  mkSetup,
  TranscriptConstraints,
) where

import Data.Aeson (withText)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import Data.Coerce (coerce)
import Data.Foldable (foldrM)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word8)
import Deriving.Aeson
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import GHC.Generics (Generic1, Par1 (..), U1 (..), type (:*:) (..))
import GHC.Natural (naturalToInteger)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.EllipticCurve.Class (compress)
import ZkFold.Algebra.Field (Zp, fromZp, toZp)
import ZkFold.Algebra.Number (KnownNat, Natural, type (^))
import qualified ZkFold.Algebra.Number as Number
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit (..))
import qualified ZkFold.ArithmeticCircuit as AC
import ZkFold.Data.Binary (toByteString)
import ZkFold.Data.Vector (Vector)
import qualified ZkFold.Data.Vector as V
import ZkFold.FFI.Rust.Plonkup (rustPlonkupProve)
import ZkFold.Prelude (log2ceiling)
import ZkFold.Protocol.NonInteractiveProof as NP (
  FromTranscript (..),
  NonInteractiveProof (..),
  ToTranscript (..),
 )
import ZkFold.Protocol.Plonkup (Plonkup (..))
import ZkFold.Protocol.Plonkup.Proof
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))
import ZkFold.Protocol.Plonkup.Relation (PlonkupRelation (..))
import ZkFold.Protocol.Plonkup.Utils (getParams, getSecretParams)
import ZkFold.Protocol.Plonkup.Verifier.Commitments
import ZkFold.Protocol.Plonkup.Verifier.Setup
import ZkFold.Protocol.Plonkup.Witness (PlonkupWitnessInput (..))
import qualified ZkFold.Symbolic.Algorithm.RSA as RSA
import ZkFold.Symbolic.Class (Symbolic (..))
import qualified ZkFold.Symbolic.Compiler as C
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Symbolic.Data.Input
import ZkFold.Symbolic.Data.UInt (OrdWord, UInt (..), exp65537Mod)
import ZkFold.Symbolic.Data.Vec (Vec (..), runVec)
import ZkFold.Symbolic.Interpreter
import ZkFold.Symbolic.MonadCircuit (newAssigned, rangeConstraint)
import Prelude hiding (Fractional (..), Num (..), length, (^))
import qualified Prelude as P

import ZkFold.Symbolic.Examples.SmartWallet (
  ByteStringFromHex (..),
  ExpModCircuitGates,
  PlonkupTs,
  TranscriptConstraints,
  ZKF (..),
  ZKProofBytes (..),
  ZKSetupBytes (..),
  mkProof,
  mkSetup,
 )

type ExpModLayout = (Vector 17 :*: Par1)

type ExpModCompiledInput = ((U1 :*: U1) :*: U1) :*: (ExpModLayout :*: U1)

type ExpModCircuit = ArithmeticCircuit Fr ExpModCompiledInput Par1

data ExpModInput c
  = ExpModInput
  { emiSig :: UInt 2048 Auto c
  , emiTokenName :: FieldElement c
  }
  deriving (Generic, Generic1)

deriving instance SymbolicData ExpModInput

deriving instance SymbolicInput ExpModInput

expModContract
  :: forall c
   . Symbolic c
  => KnownNat (NumberOfRegisters (BaseField c) 4096 Auto)
  => KnownNat (Ceil (GetRegisterSize (BaseField c) 4096 Auto) OrdWord)
  => RSA.PublicKey 2048 c
  -> ExpModInput c
  -> FieldElement c
expModContract RSA.PublicKey {..} (ExpModInput sig tokenNameAsFE) = hashAsFE * tokenNameAsFE
 where
  msgHash :: UInt 2048 Auto c
  msgHash = exp65537Mod @c @2048 @2048 sig pubN

  unpadded :: UInt 256 Auto c
  unpadded = resize msgHash

  rsize :: Natural
  rsize = 2 ^ registerSize @(BaseField c) @256 @Auto

  -- UInt `mod` BLS12_381_Scalar is just weighted sum of its registers
  --
  hashAsFE :: FieldElement c
  hashAsFE = FieldElement $ fromCircuitF (let UInt regs = unpadded in regs) $ \v -> do
    z <- newAssigned (const zero)
    ans <- foldrM (\i acc -> newAssigned $ \p -> scale rsize (p acc) + p i) z v
    pure $ Par1 ans

expModCircuit :: Natural -> Natural -> ExpModCircuit
expModCircuit pubE' pubN' = runVec $ C.compile @Fr $ expModContract (RSA.PublicKey {..})
 where
  pubE = fromConstant pubE'
  pubN = fromConstant pubN'

expModSetup
  :: forall t
   . TranscriptConstraints t
  => Fr
  -> ExpModCircuit
  -> SetupVerify (PlonkupTs ExpModCompiledInput ExpModCircuitGates t)
expModSetup x ac = setupV
 where
  (omega, k1, k2) = getParams (Number.value @ExpModCircuitGates)
  (gs, h1) = getSecretParams @ExpModCircuitGates @BLS12_381_G1_JacobianPoint @BLS12_381_G2_JacobianPoint x
  plonkup = Plonkup omega k1 k2 ac h1 gs
  setupV = setupVerify @(PlonkupTs ExpModCompiledInput ExpModCircuitGates t) plonkup

data ExpModProofInput
  = ExpModProofInput
  { piPubE :: Natural
  , piPubN :: Natural
  , piSignature :: Natural
  , piTokenName :: Natural
  }
  deriving (Generic, P.Show)

deriving instance ToJSON ExpModProofInput

deriving instance FromJSON ExpModProofInput

expModProof
  :: forall t
   . TranscriptConstraints t
  => Fr
  -> PlonkupProverSecret BLS12_381_G1_JacobianPoint
  -> ExpModCircuit
  -> ExpModProofInput
  -> Proof (PlonkupTs ExpModCompiledInput ExpModCircuitGates t)
expModProof x ps ac ExpModProofInput {..} = proof
 where
  input :: ExpModInput (Interpreter Fr)
  input =
    ExpModInput
      (fromConstant piSignature)
      (fromConstant piTokenName)

  witnessInputs :: ExpModLayout Fr
  witnessInputs = runInterpreter $ arithmetize input

  paddedWitnessInputs :: ExpModCompiledInput Fr
  paddedWitnessInputs = ((U1 :*: U1) :*: U1) :*: (witnessInputs :*: U1)

  (omega, k1, k2) = getParams (Number.value @ExpModCircuitGates)
  (gs, h1) = getSecretParams @ExpModCircuitGates @BLS12_381_G1_JacobianPoint @BLS12_381_G2_JacobianPoint x
  plonkup = Plonkup omega k1 k2 ac h1 gs :: PlonkupTs ExpModCompiledInput ExpModCircuitGates t
  setupP = setupProve @(PlonkupTs ExpModCompiledInput ExpModCircuitGates t) plonkup
  witness = (PlonkupWitnessInput @ExpModCompiledInput @BLS12_381_G1_JacobianPoint paddedWitnessInputs, ps)
  (_, proof) = prove @(PlonkupTs ExpModCompiledInput ExpModCircuitGates t) setupP witness
