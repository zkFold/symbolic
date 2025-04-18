{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Base.Protocol.Plonkup (
    module ZkFold.Base.Protocol.Plonkup.Internal,
    Plonkup (..)
) where

import           Control.DeepSeq                                     (NFData)
import           Data.Binary                                         (Binary)
import           Data.Functor.Rep                                    (Rep, Representable)
import qualified Data.Vector                                         as V
import           Data.Word                                           (Word8)
import           Prelude                                             hiding (Num (..), div, drop, length, replicate,
                                                                      sum, take, (!!), (/), (^))
import qualified Prelude                                             as P hiding (length)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.Class             (Compressible (..), CyclicGroup (..), Pairing (..))
import           ZkFold.Base.Algebra.Polynomials.Univariate
import           ZkFold.Base.Protocol.NonInteractiveProof
import           ZkFold.Base.Protocol.Plonkup.Input
import           ZkFold.Base.Protocol.Plonkup.Internal
import           ZkFold.Base.Protocol.Plonkup.Proof
import           ZkFold.Base.Protocol.Plonkup.Prover
import           ZkFold.Base.Protocol.Plonkup.Setup
import           ZkFold.Base.Protocol.Plonkup.Verifier
import           ZkFold.Base.Protocol.Plonkup.Witness
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal

{-| Based on the paper https://eprint.iacr.org/2022/086.pdf -}

instance forall i n l g1 g2 gt ts pv .
        ( KnownNat n
        , Representable i
        , Representable l
        , Foldable l
        , Ord (Rep i)
        , Pairing g1 g2 gt
        , Compressible g1
        , Eq gt
        , Arithmetic (ScalarFieldOf g1)
        , Binary (ScalarFieldOf g2)
        , ToTranscript ts Word8
        , ToTranscript ts (ScalarFieldOf g1)
        , ToTranscript ts (Compressed g1)
        , FromTranscript ts (ScalarFieldOf g1)
        , StrictMultiplicativeSemigroup (pv (PlonkupPolyExtendedLength n))
        , Bilinear (V.Vector g1) (pv (PlonkupPolyExtendedLength n)) g1
        , NFData (pv (PlonkupPolyExtendedLength n))
        , KnownNat (PlonkupPolyExtendedLength n)
        , UnivariateFieldPolyVec (ScalarFieldOf g2) pv
        ) => NonInteractiveProof (Plonkup i n l g1 g2 ts pv) where
    type Transcript (Plonkup i n l g1 g2 ts pv)  = ts
    type SetupProve (Plonkup i n l g1 g2 ts pv)  = PlonkupProverSetup i n l g1 g2 pv
    type SetupVerify (Plonkup i n l g1 g2 ts pv) = PlonkupVerifierSetup i n l g1 g2 pv
    type Witness (Plonkup i n l g1 g2 ts pv)     = (PlonkupWitnessInput i g1, PlonkupProverSecret g1)
    type Input (Plonkup i n l g1 g2 ts pv)       = PlonkupInput l g1
    type Proof (Plonkup i n l g1 g2 ts pv)       = PlonkupProof g1

    setupProve ::
      Plonkup i n l g1 g2 ts pv -> SetupProve (Plonkup i n l g1 g2 ts pv)
    setupProve plonk =
        let PlonkupSetup {..} = plonkupSetup @i @n @l @g1 @g2 @gt @ts plonk
        in PlonkupProverSetup {..}

    setupVerify ::
      Plonkup i n l g1 g2 ts pv -> SetupVerify (Plonkup i n l g1 g2 ts pv)
    setupVerify plonk =
        let PlonkupSetup {..} = plonkupSetup @i @n @l @g1 @g2 @gt @ts plonk
        in PlonkupVerifierSetup {..}

    prove ::
      SetupProve (Plonkup i n l g1 g2 ts pv) ->
      Witness (Plonkup i n l g1 g2 ts pv) ->
      (Input (Plonkup i n l g1 g2 ts pv), Proof (Plonkup i n l g1 g2 ts pv))
    prove setup witness =
        let (input, proof, _) = with4n6 @n (plonkupProve @i @n @l @g1 @g2 @ts @pv setup witness)
        in (input, proof)

    verify ::
      SetupVerify (Plonkup i n l g1 g2 ts pv) ->
      Input (Plonkup i n l g1 g2 ts pv) ->
      Proof (Plonkup i n l g1 g2 ts pv) -> Bool
    verify = with4n6 @n $ plonkupVerify @i @n @l @g1 @g2 @gt @ts @pv
