{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Cardano.Contracts.SmartWallet
    ( expModCircuit
    , expModSetup
    , expModProof
    ) where

import           Data.Foldable                               (foldrM)
import           Data.Proxy
import           Data.Word                                   (Word8)
import           GHC.Generics                                (Generic, Par1 (..), U1 (..), type (:*:) (..))
import           Prelude                                     (const, ($), (<$>))

import           ZkFold.Base.Algebra.Basic.Class
import qualified ZkFold.Base.Algebra.Basic.Number            as Number
import           ZkFold.Base.Algebra.Basic.Number            (Natural, type (^))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_CompressedPoint, BLS12_381_G1_Point,
                                                              BLS12_381_G2_Point, Fr)
import           ZkFold.Base.Algebra.Polynomials.Univariate  (PolyVec)
import           ZkFold.Base.Data.Vector                     (Vector)
import           ZkFold.Base.Protocol.NonInteractiveProof
import           ZkFold.Base.Protocol.Plonkup                (Plonkup (..))
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret (..))
import           ZkFold.Base.Protocol.Plonkup.Utils          (getParams, getSecrectParams)
import           ZkFold.Base.Protocol.Plonkup.Witness        (PlonkupWitnessInput (..))
import qualified ZkFold.Symbolic.Algorithms.RSA              as RSA
import           ZkFold.Symbolic.Class                       (Symbolic (..))
import qualified ZkFold.Symbolic.Compiler                    as C
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..))
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.FieldElement
import           ZkFold.Symbolic.Data.Input
import           ZkFold.Symbolic.Data.UInt
import           ZkFold.Symbolic.Interpreter
import           ZkFold.Symbolic.MonadCircuit                (newAssigned)

type NGates = 2^19

type ExpModLayout = (((Vector 1
                                  :*: Vector 17)
                                 :*: (Vector 17 :*: Par1))
                                :*: U1)
type ExpModCompiledInput = (((U1 :*: U1) :*: (U1 :*: U1)) :*: U1)
                           :*: ExpModLayout

type PlonkupTs t = Plonkup ExpModCompiledInput NGates Par1 BLS12_381_G1_Point BLS12_381_G2_Point t (PolyVec Fr)

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

deriving instance
    ( Symbolic ctx
    , KnownRegisters ctx RSA.PubExponentSize 'Auto
    , KnownRegisters ctx 2048 'Auto
    ) => SymbolicInput (ExpModInput ctx)

expModCircuit
    :: forall c
    .  Symbolic c
    => ExpModInput c
    -> FieldElement c
expModCircuit (ExpModInput RSA.PublicKey{..} sig tokenName) = hashAsFE * tokenName
    where
        msgHash :: UInt 2048 Auto c
        msgHash = expMod @c @2048 @RSA.PubExponentSize @2048 sig pubE pubN

        rsize :: Natural
        rsize = registerSize @(BaseField c) @2048 @Auto

        -- UInt `mod` BLS12_381_Scalar is just weighted sum of its registers
        --
        hashAsFE :: FieldElement c
        hashAsFE = FieldElement $ fromCircuitF (let UInt regs = msgHash in regs) $ \v -> do
            z <- newAssigned (const zero)
            Par1 <$> foldrM (\a i -> newAssigned $ \p -> scale rsize (p a) + p i) z v

expModSetup :: forall t .  TranscriptConstraints t => Fr -> SetupVerify (PlonkupTs t)
expModSetup x = setupV
    where
        ac = C.compile @Fr expModCircuit

        (omega, k1, k2) = getParams (Number.value @NGates)
        (gs, h1) = getSecrectParams @NGates @BLS12_381_G1_Point @BLS12_381_G2_Point x
        plonkup = Plonkup omega k1 k2 ac h1 gs
        setupV  = setupVerify @(PlonkupTs t) plonkup

data ExpModProofInput =
    ExpModProofInput
        { piPubE      :: Natural
        , piPubN      :: Natural
        , piSignature :: Natural
        , piTokenName :: Natural
        }

expModProof
    :: forall t
    .  TranscriptConstraints t 
    => Fr
    -> PlonkupProverSecret BLS12_381_G1_Point
    -> ExpModProofInput
    -> Proof (PlonkupTs t)
expModProof x ps ExpModProofInput{..} = proof
    where
        ac = C.compile @Fr expModCircuit

        input :: ExpModInput (Interpreter Fr)
        input =
            ExpModInput
                (RSA.PublicKey
                    (fromConstant piPubE)
                    (fromConstant piPubN))
                (fromConstant piSignature)
                (fromConstant piTokenName)

        witnessInputs   = runInterpreter $ arithmetize input Proxy

        (omega, k1, k2) = getParams (Number.value @NGates)
        (gs, h1) = getSecrectParams @NGates @BLS12_381_G1_Point @BLS12_381_G2_Point x
        plonkup = Plonkup omega k1 k2 ac h1 gs :: PlonkupTs t
        setupP  = setupProve @(PlonkupTs t) plonkup
        witness = (PlonkupWitnessInput @ExpModLayout @BLS12_381_G1_Point (witnessInputs :*: U1), ps)
        (_, proof) = prove @(PlonkupTs t) setupP witness
