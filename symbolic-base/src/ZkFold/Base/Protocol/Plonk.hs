{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Protocol.Plonk (
    Plonk (..)
) where

import           Data.Binary                                         (Binary)
import           Data.Functor.Classes                                (Show1)
import           Data.Functor.Rep                                    (Rep, Representable)
import           Data.Kind                                           (Type)
import qualified Data.Vector                                         as V
import           Data.Word                                           (Word8)
import           Prelude                                             hiding (Num (..), div, drop, length, replicate,
                                                                      sum, take, (!!), (/), (^))
import qualified Prelude                                             as P hiding (length)
import           Test.QuickCheck                                     (Arbitrary (..))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.Polynomials.Univariate          hiding (qr)
import           ZkFold.Base.Data.Vector                             (Vector)
import           ZkFold.Base.Protocol.NonInteractiveProof
import           ZkFold.Base.Protocol.Plonk.Prover                   (plonkProve)
import           ZkFold.Base.Protocol.Plonk.Verifier                 (plonkVerify)
import           ZkFold.Base.Protocol.Plonkup.Input
import           ZkFold.Base.Protocol.Plonkup.Internal
import           ZkFold.Base.Protocol.Plonkup.Proof
import           ZkFold.Base.Protocol.Plonkup.Prover
import           ZkFold.Base.Protocol.Plonkup.Verifier
import           ZkFold.Base.Protocol.Plonkup.Witness
import           ZkFold.Symbolic.Compiler                            (desugarRanges)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal
import GHC.Generics ((:*:))


{-| Based on the paper https://eprint.iacr.org/2019/953.pdf -}

data Plonk i (n :: Natural) w l g1 g2 transcript pv = Plonk {
        omega :: ScalarFieldOf g1,
        k1    :: ScalarFieldOf g1,
        k2    :: ScalarFieldOf g1,
        ac    :: ArithmeticCircuit (ScalarFieldOf g1) i (w :*: l),
        h1    :: g2,
        gs'   :: Vector (n + 5) g1
    }

fromPlonkup ::
    ( Arithmetic (ScalarFieldOf g1), Binary (ScalarFieldOf g1)
    , Binary (Rep i), Ord (Rep i), Representable i
    ) => Plonkup i n w l g1 g2 ts pv -> Plonk i n w l g1 g2 ts pv
fromPlonkup Plonkup {..} = Plonk { ac = desugarRanges ac, ..}

toPlonkup :: Plonk i n w l g1 g2 ts pv -> Plonkup i n w l g1 g2 ts pv
toPlonkup Plonk {..} = Plonkup {..}

instance ( Show1 w, Show1 l, Show (Rep i), Show (ScalarFieldOf g1)
         , Ord (Rep i), Show g1, Show g2) => Show (Plonk i n w l g1 g2 t pv) where
    show Plonk {..} =
        "Plonk: " ++ show omega ++ " " ++ show k1 ++ " " ++ show k2 ++ " "
                  ++ show (acOutput ac) ++ " " ++ show ac ++ " "
                  ++ show h1 ++ " " ++ show gs'

instance ( Arithmetic (ScalarFieldOf g1), Binary (ScalarFieldOf g1)
         , Binary (Rep i), Ord (Rep i), Representable i
         , Arbitrary (Plonkup i n w l g1 g2 t pv))
        => Arbitrary (Plonk i n w l g1 g2 t pv) where
    arbitrary = fromPlonkup <$> arbitrary

instance forall i n w l g1 g2 gt (ts :: Type) pv .
        ( NonInteractiveProof (Plonkup i n w l g1 g2 ts pv)
        , SetupProve (Plonkup i n w l g1 g2 ts pv) ~ PlonkupProverSetup i n w l g1 g2 pv
        , SetupVerify (Plonkup i n w l g1 g2 ts pv) ~ PlonkupVerifierSetup i n w l g1 g2 pv
        , Witness (Plonkup i n w l g1 g2 ts pv) ~ (PlonkupWitnessInput i g1, PlonkupProverSecret g1)
        , Input (Plonkup i n w l g1 g2 ts pv) ~ PlonkupInput l g1
        , Proof (Plonkup i n w l g1 g2 ts pv) ~ PlonkupProof g1
        , Foldable l
        , Compressible g1
        , Pairing g1 g2 gt
        , Eq gt
        , Arithmetic (ScalarFieldOf g1)
        , ToTranscript ts Word8
        , ToTranscript ts (ScalarFieldOf g1)
        , ToTranscript ts (Compressed g1)
        , FromTranscript ts (ScalarFieldOf g1)
        , Bilinear (V.Vector g1) (pv (PlonkupPolyExtendedLength n)) g1
        , KnownNat n
        , KnownNat (PlonkupPolyExtendedLength n)
        , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
        ) => NonInteractiveProof (Plonk i n w l g1 g2 ts pv) where
    type Transcript (Plonk i n w l g1 g2 ts pv)  = ts
    type SetupProve (Plonk i n w l g1 g2 ts pv)  = PlonkupProverSetup i n w l g1 g2 pv
    type SetupVerify (Plonk i n w l g1 g2 ts pv) = PlonkupVerifierSetup i n w l g1 g2 pv
    type Witness (Plonk i n w l g1 g2 ts pv)     = (PlonkupWitnessInput i g1, PlonkupProverSecret g1)
    type Input (Plonk i n w l g1 g2 ts pv)       = PlonkupInput l g1
    type Proof (Plonk i n w l g1 g2 ts pv)       = PlonkupProof g1

    setupProve ::
      Plonk i n w l g1 g2 ts pv -> SetupProve (Plonk i n w l g1 g2 ts pv)
    setupProve = setupProve @(Plonkup i n w l g1 g2 ts pv) . toPlonkup

    setupVerify ::
      Plonk i n w l g1 g2 ts pv -> SetupVerify (Plonk i n w l g1 g2 ts pv)
    setupVerify = setupVerify @(Plonkup i n w l g1 g2 ts pv) . toPlonkup

    prove ::
      SetupProve (Plonk i n w l g1 g2 ts pv) ->
      Witness (Plonk i n w l g1 g2 ts pv) ->
      (Input (Plonk i n w l g1 g2 ts pv), Proof (Plonk i n w l g1 g2 ts pv))
    prove setup witness =
        let (input, proof, _) = plonkProve @i @n @w @l @g1 @g2 @ts @pv setup witness
        in (input, proof)

    verify ::
      SetupVerify (Plonk i n w l g1 g2 ts pv) ->
      Input (Plonk i n w l g1 g2 ts pv) -> Proof (Plonk i n w l g1 g2 ts pv) -> Bool
    verify = plonkVerify @i @n @w @l @g1 @g2 @gt @ts @pv
