{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonk (
    Plonk (..)
) where

import           Data.Binary                                        (Binary)
import           Data.Functor.Classes                               (Show1)
import           Data.Kind                                          (Type)
import qualified Data.Vector                                        as V
import           Data.Word                                          (Word8)
import           Prelude                                            hiding (Num (..), div, drop, length, replicate, sum,
                                                                     take, (!!), (/), (^))
import qualified Prelude                                            as P hiding (length)
import           Test.QuickCheck                                    (Arbitrary (..))

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.Class
import           ZkFold.Algebra.Number
import           ZkFold.Algebra.Polynomial.Univariate               hiding (qr)
import           ZkFold.Data.Vector                                 (Vector)
import           ZkFold.Protocol.NonInteractiveProof
import           ZkFold.Protocol.Plonk.Prover                       (plonkProve)
import           ZkFold.Protocol.Plonk.Verifier                     (plonkVerify)
import           ZkFold.Protocol.Plonkup.Input
import           ZkFold.Protocol.Plonkup.Internal
import           ZkFold.Protocol.Plonkup.Proof
import           ZkFold.Protocol.Plonkup.Prover
import           ZkFold.Protocol.Plonkup.Verifier
import           ZkFold.Protocol.Plonkup.Witness
import           ZkFold.Symbolic.Class                              (Arithmetic)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit         (ArithmeticCircuit (acContext), desugarRanges)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context (acOutput)

{-| Based on the paper https://eprint.iacr.org/2019/953.pdf -}

data Plonk i o (n :: Natural) g1 g2 transcript pv = Plonk {
        omega :: ScalarFieldOf g1,
        k1    :: ScalarFieldOf g1,
        k2    :: ScalarFieldOf g1,
        ac    :: ArithmeticCircuit (ScalarFieldOf g1) i o,
        h1    :: g2,
        gs'   :: Vector (n + 5) (ScalarFieldOf g1),
        g1    :: g1
    }

fromPlonkup ::
    (Arithmetic (ScalarFieldOf g1), Binary (ScalarFieldOf g1)) =>
    Plonkup i o n g1 g2 ts pv -> Plonk i o n g1 g2 ts pv
fromPlonkup Plonkup {..} = Plonk { ac = desugarRanges ac, ..}

toPlonkup :: Plonk i o n g1 g2 ts pv -> Plonkup i o n g1 g2 ts pv
toPlonkup Plonk {..} = Plonkup {..}

instance (Show1 o, Show (ScalarFieldOf g1), Show g1, Show g2) =>
            Show (Plonk i o n g1 g2 t pv) where
    show Plonk {..} =
        "Plonk: " ++ show omega ++ " " ++ show k1 ++ " " ++ show k2 ++ " "
                  ++ show (acOutput (acContext ac)) ++ " " ++ show ac ++ " "
                  ++ show h1 ++ " " ++ show gs'

instance ( Arithmetic (ScalarFieldOf g1), Binary (ScalarFieldOf g1)
         , Arbitrary (Plonkup i o n g1 g2 t pv))
        => Arbitrary (Plonk i o n g1 g2 t pv) where
    arbitrary = fromPlonkup <$> arbitrary

instance forall i o n g1 g2 gt (ts :: Type) pv .
        ( NonInteractiveProof (Plonkup i o n g1 g2 ts pv)
        , SetupProve (Plonkup i o n g1 g2 ts pv) ~ PlonkupProverSetup i o n g1 g2 pv
        , SetupVerify (Plonkup i o n g1 g2 ts pv) ~ PlonkupVerifierSetup i o n g1 g2 pv
        , Witness (Plonkup i o n g1 g2 ts pv) ~ (PlonkupWitnessInput i g1, PlonkupProverSecret g1)
        , Input (Plonkup i o n g1 g2 ts pv) ~ PlonkupInput g1
        , Proof (Plonkup i o n g1 g2 ts pv) ~ PlonkupProof g1
        , Compressible g1
        , Pairing g1 g2 gt
        , Eq gt
        , Arithmetic (ScalarFieldOf g1)
        , ToTranscript ts Word8
        , ToTranscript ts (ScalarFieldOf g1)
        , ToTranscript ts (Compressed g1)
        , FromTranscript ts (ScalarFieldOf g1)
        , Bilinear (V.Vector (ScalarFieldOf g1)) (pv (PlonkupPolyExtendedLength n)) (ScalarFieldOf g1)
        , KnownNat n
        , KnownNat (PlonkupPolyExtendedLength n)
        , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
        ) => NonInteractiveProof (Plonk i o n g1 g2 ts pv) where
    type Transcript (Plonk i o n g1 g2 ts pv)  = ts
    type SetupProve (Plonk i o n g1 g2 ts pv)  = PlonkupProverSetup i o n g1 g2 pv
    type SetupVerify (Plonk i o n g1 g2 ts pv) = PlonkupVerifierSetup i o n g1 g2 pv
    type Witness (Plonk i o n g1 g2 ts pv)     = (PlonkupWitnessInput i g1, PlonkupProverSecret g1)
    type Input (Plonk i o n g1 g2 ts pv)       = PlonkupInput g1
    type Proof (Plonk i o n g1 g2 ts pv)       = PlonkupProof g1

    setupProve ::
      Plonk i o n g1 g2 ts pv -> SetupProve (Plonk i o n g1 g2 ts pv)
    setupProve = setupProve @(Plonkup i o n g1 g2 ts pv) . toPlonkup

    setupVerify ::
      Plonk i o n g1 g2 ts pv -> SetupVerify (Plonk i o n g1 g2 ts pv)
    setupVerify = setupVerify @(Plonkup i o n g1 g2 ts pv) . toPlonkup

    prove ::
      SetupProve (Plonk i o n g1 g2 ts pv) ->
      Witness (Plonk i o n g1 g2 ts pv) ->
      (Input (Plonk i o n g1 g2 ts pv), Proof (Plonk i o n g1 g2 ts pv))
    prove setup witness =
        let (input, proof, _) = plonkProve @i @o @n @g1 @g2 @ts @pv setup witness
        in (input, proof)

    verify ::
      SetupVerify (Plonk i o n g1 g2 ts pv) ->
      Input (Plonk i o n g1 g2 ts pv) -> Proof (Plonk i o n g1 g2 ts pv) -> Bool
    verify = plonkVerify @i @o @n @g1 @g2 @gt @ts @pv
