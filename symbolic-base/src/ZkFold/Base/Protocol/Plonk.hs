{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Protocol.Plonk (
    Plonk (..)
) where

import           Data.Binary                                         (Binary)
import           Data.Functor.Classes                                (Show1)
import           Data.Functor.Rep                                    (Rep)
import           Data.Kind                                           (Type)
import           Data.Word                                           (Word8)
import           Prelude                                             hiding (Num (..), div, drop, length, replicate,
                                                                      sum, take, (!!), (/), (^))
import qualified Prelude                                             as P hiding (length)
import           Test.QuickCheck                                     (Arbitrary (..))

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


{-| Based on the paper https://eprint.iacr.org/2019/953.pdf -}

data Plonk p i (n :: Natural) l g1 g2 transcript pv = Plonk {
        omega :: ScalarFieldOf g1,
        k1    :: ScalarFieldOf g1,
        k2    :: ScalarFieldOf g1,
        ac    :: ArithmeticCircuit (ScalarFieldOf g1) p i l,
        h1    :: g2,
        gs'   :: Vector (n + 5) g1
    }

fromPlonkup ::
    ( Arithmetic (ScalarFieldOf g1)
    , Binary (ScalarFieldOf g1)
    , Binary (Rep p)
    , Binary (Rep i)
    , Ord (Rep i)
    ) => Plonkup p i n l g1 g2 ts pv -> Plonk p i n l g1 g2 ts pv
fromPlonkup Plonkup {..} = Plonk { ac = desugarRanges ac, ..}

toPlonkup :: Plonk p i n l g1 g2 ts pv -> Plonkup p i n l g1 g2 ts pv
toPlonkup Plonk {..} = Plonkup {..}

instance (Show1 l, Show (Rep i), Show (ScalarFieldOf g1), Ord (Rep i), Show g1, Show g2) => Show (Plonk p i n l g1 g2 t pv) where
    show Plonk {..} =
        "Plonk: " ++ show omega ++ " " ++ show k1 ++ " " ++ show k2 ++ " " ++ show (acOutput ac) ++ " " ++ show ac ++ " " ++ show h1 ++ " " ++ show gs'

instance ( Arithmetic (ScalarFieldOf g1), Binary (ScalarFieldOf g1)
         , Binary (Rep p), Binary (Rep i), Ord (Rep i)
         , Arbitrary (Plonkup p i n l g1 g2 t pv))
        => Arbitrary (Plonk p i n l g1 g2 t pv) where
    arbitrary = fromPlonkup <$> arbitrary

instance forall p i n l g1 g2 gt (ts :: Type) core pv .
        ( NonInteractiveProof (Plonkup p i n l g1 g2 ts pv) core
        , SetupProve (Plonkup p i n l g1 g2 ts pv) ~ PlonkupProverSetup p i n l g1 g2 pv
        , SetupVerify (Plonkup p i n l g1 g2 ts pv) ~ PlonkupVerifierSetup p i n l g1 g2 pv
        , Witness (Plonkup p i n l g1 g2 ts pv) ~ (PlonkupWitnessInput p i g1, PlonkupProverSecret g1)
        , Input (Plonkup p i n l g1 g2 ts pv) ~ PlonkupInput l g1
        , Proof (Plonkup p i n l g1 g2 ts pv) ~ PlonkupProof g1
        , Foldable l
        , Compressible g1
        , Pairing g1 g2 gt
        , Eq gt
        , Arithmetic (ScalarFieldOf g1)
        , ToTranscript ts Word8
        , ToTranscript ts (ScalarFieldOf g1)
        , ToTranscript ts (Compressed g1)
        , FromTranscript ts (ScalarFieldOf g1)
        , CoreFunction g1 core pv (PlonkupPolyExtendedLength n)
        , UnivariateFieldPolyVec pv (ScalarFieldOf g1) n
        , UnivariateFieldPolyVec pv (ScalarFieldOf g1) (PlonkupPolyExtendedLength n)
        ) => NonInteractiveProof (Plonk p i n l g1 g2 ts pv) core where
    type Transcript (Plonk p i n l g1 g2 ts pv)  = ts
    type SetupProve (Plonk p i n l g1 g2 ts pv)  = PlonkupProverSetup p i n l g1 g2 pv
    type SetupVerify (Plonk p i n l g1 g2 ts pv) = PlonkupVerifierSetup p i n l g1 g2 pv
    type Witness (Plonk p i n l g1 g2 ts pv)     = (PlonkupWitnessInput p i g1, PlonkupProverSecret g1)
    type Input (Plonk p i n l g1 g2 ts pv)       = PlonkupInput l g1
    type Proof (Plonk p i n l g1 g2 ts pv)       = PlonkupProof g1

    setupProve :: Plonk p i n l g1 g2 ts pv -> SetupProve (Plonk p i n l g1 g2 ts pv)
    setupProve = setupProve @(Plonkup p i n l g1 g2 ts pv) @core . toPlonkup

    setupVerify :: Plonk p i n l g1 g2 ts pv -> SetupVerify (Plonk p i n l g1 g2 ts pv)
    setupVerify = setupVerify @(Plonkup p i n l g1 g2 ts pv) @core . toPlonkup

    prove :: SetupProve (Plonk p i n l g1 g2 ts pv) -> Witness (Plonk p i n l g1 g2 ts pv) -> (Input (Plonk p i n l g1 g2 ts pv), Proof (Plonk p i n l g1 g2 ts pv))
    prove setup witness =
        let (input, proof, _) = plonkProve @p @i @n @l @g1 @g2 @ts @core @pv setup witness
        in (input, proof)

    verify :: SetupVerify (Plonk p i n l g1 g2 ts pv) -> Input (Plonk p i n l g1 g2 ts pv) -> Proof (Plonk p i n l g1 g2 ts pv) -> Bool
    verify = plonkVerify @p @i @n @l @g1 @g2 @gt @ts @pv
