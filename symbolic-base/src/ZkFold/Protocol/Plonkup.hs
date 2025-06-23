{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Protocol.Plonkup (
  module ZkFold.Protocol.Plonkup.Internal,
  Plonkup (..),
) where

import Data.Binary (Binary)
import Data.Functor.Rep (Rep, Representable)
import qualified Data.Vector as V
import Data.Word (Word8)
import Prelude hiding (
  Num (..),
  div,
  drop,
  length,
  replicate,
  sum,
  take,
  (!!),
  (/),
  (^),
 )
import qualified Prelude as P hiding (length)

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (Compressible (..), CyclicGroup (..), Pairing (..))
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Univariate
import ZkFold.FFI.Rust.Conversion
import ZkFold.Protocol.NonInteractiveProof
import ZkFold.Protocol.Plonkup.Input
import ZkFold.Protocol.Plonkup.Internal
import ZkFold.Protocol.Plonkup.Proof
import ZkFold.Protocol.Plonkup.Prover
import ZkFold.Protocol.Plonkup.Setup
import ZkFold.Protocol.Plonkup.Verifier
import ZkFold.Protocol.Plonkup.Witness
import ZkFold.Symbolic.Class (Arithmetic)

-- | Based on the paper https://eprint.iacr.org/2022/086.pdf
instance
  forall i o n g1 g2 gt ts pv rustG1 rustPv
   . -- instance forall i o n g1 g2 gt ts pv .
  ( KnownNat n
  , Representable i
  , Representable o
  , Foldable o
  , Pairing g1 g2 gt
  , Compressible g1
  , Eq gt
  , Arithmetic (ScalarFieldOf g1)
  , Binary (Rep i)
  , ToTranscript ts Word8
  , ToTranscript ts (ScalarFieldOf g1)
  , ToTranscript ts (Compressed g1)
  , FromTranscript ts (ScalarFieldOf g1)
  , Bilinear (V.Vector rustG1) (pv (PlonkupPolyExtendedLength n)) g1
  , KnownNat (PlonkupPolyExtendedLength n)
  , UnivariateFieldPolyVec (ScalarFieldOf g2) pv
  , UnivariateFieldPolyVec (ScalarFieldOf rustG1) rustPv
  , RustHaskell (ScalarFieldOf rustG1) (ScalarFieldOf g1)
  , RustHaskell rustG1 g1
  , RustHaskell (rustPv (PlonkupPolyExtendedLength n)) (pv (PlonkupPolyExtendedLength n))
  )
  => NonInteractiveProof (Plonkup i o n g1 g2 ts pv)
  where
  type Transcript (Plonkup i o n g1 g2 ts pv) = ts
  type SetupProve (Plonkup i o n g1 g2 ts pv) = PlonkupProverSetup i o n g1 g2 pv
  type SetupVerify (Plonkup i o n g1 g2 ts pv) = PlonkupVerifierSetup i o n g1 g2 pv
  type Witness (Plonkup i o n g1 g2 ts pv) = (PlonkupWitnessInput i g1, PlonkupProverSecret g1)
  type Input (Plonkup i o n g1 g2 ts pv) = PlonkupInput g1
  type Proof (Plonkup i o n g1 g2 ts pv) = PlonkupProof g1

  setupProve
    :: Plonkup i o n g1 g2 ts pv -> SetupProve (Plonkup i o n g1 g2 ts pv)
  setupProve plonk =
    let PlonkupSetup {..} = plonkupSetup @i @o @n @g1 @g2 @gt @ts plonk
     in PlonkupProverSetup {..}

  setupVerify
    :: Plonkup i o n g1 g2 ts pv -> SetupVerify (Plonkup i o n g1 g2 ts pv)
  setupVerify plonk =
    let PlonkupSetup {..} = plonkupSetup @i @o @n @g1 @g2 @gt @ts plonk
     in PlonkupVerifierSetup {..}

  prove
    :: SetupProve (Plonkup i o n g1 g2 ts pv)
    -> Witness (Plonkup i o n g1 g2 ts pv)
    -> (Input (Plonkup i o n g1 g2 ts pv), Proof (Plonkup i o n g1 g2 ts pv))
  prove setup witness =
    let (input, proof, _) = with4n6 @n (plonkupProve @i @o @n @g1 @g2 @ts @pv @rustG1 @rustPv setup witness)
     in --        let (input, proof, _) = with4n6 @n (plonkupProve @i @o @n @g1 @g2 @ts @pv setup witness)
        (input, proof)

  verify
    :: SetupVerify (Plonkup i o n g1 g2 ts pv)
    -> Input (Plonkup i o n g1 g2 ts pv)
    -> Proof (Plonkup i o n g1 g2 ts pv)
    -> Bool
  verify = with4n6 @n $ plonkupVerify @i @o @n @g1 @g2 @gt @ts @pv
