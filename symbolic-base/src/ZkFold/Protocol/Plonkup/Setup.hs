{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Setup where

import           Data.Binary                                  (Binary)
import           Data.Functor.Rep                             (Rep, Representable)
import           Data.Maybe                                   (fromJust)
import qualified Data.Vector                                  as V
import           Prelude                                      hiding (Num (..), drop, length, sum, take, (!!), (/), (^))

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.Class           (CyclicGroup (..), Pairing)
import           ZkFold.Algebra.Number                        (KnownNat, value)
import           ZkFold.Algebra.Permutation                   (fromPermutation)
import           ZkFold.Algebra.Polynomial.Univariate         (UnivariateFieldPolyVec (..), polyVecInLagrangeBasis,
                                                               toPolyVec)
import           ZkFold.Data.Vector                           (Vector (..))
import           ZkFold.Protocol.Plonkup.Internal             (Plonkup (..), PlonkupPermutationSize,
                                                               PlonkupPolyExtendedLength)
import           ZkFold.Protocol.Plonkup.Prover.Polynomials   (PlonkupCircuitPolynomials (..))
import           ZkFold.Protocol.Plonkup.Relation             (PlonkupRelation (..), toPlonkupRelation)
import           ZkFold.Protocol.Plonkup.Verifier.Commitments (PlonkupCircuitCommitments (..))
import           ZkFold.Symbolic.Class                        (Arithmetic)

data PlonkupSetup i n l g1 g2 pv = PlonkupSetup
    { omega       :: ScalarFieldOf g1
    , k1          :: ScalarFieldOf g1
    , k2          :: ScalarFieldOf g1
    , gs          :: V.Vector g1
    , h0          :: g2
    , h1          :: g2
    , sigma1s     :: pv n
    , sigma2s     :: pv n
    , sigma3s     :: pv n
    , relation    :: PlonkupRelation i n l (ScalarFieldOf g1) pv
    , polynomials :: PlonkupCircuitPolynomials n g1 pv
    , commitments :: PlonkupCircuitCommitments g1
    }

instance
        ( Show g1
        , Show g2
        , Show (ScalarFieldOf g1)
        , Show (pv n)
        , Show (pv (PlonkupPolyExtendedLength n))
        , Show (PlonkupRelation i n l (ScalarFieldOf g1) pv)
        ) => Show (PlonkupSetup i n l g1 g2 pv) where
    show PlonkupSetup {..} =
        "Setup: "
        ++ show omega ++ " "
        ++ show k1 ++ " "
        ++ show k2 ++ " "
        ++ show gs ++ " "
        ++ show h0 ++ " "
        ++ show h1 ++ " "
        ++ show sigma1s ++ " "
        ++ show sigma2s ++ " "
        ++ show sigma3s ++ " "
        ++ show relation ++ " "
        ++ show polynomials ++ " "
        ++ show commitments

plonkupSetup :: forall i n l g1 g2 gt ts pv .
    ( Representable i
    , Representable l
    , Foldable l
    , Ord (Rep i)
    , Arithmetic (ScalarFieldOf g1)
    , Binary (ScalarFieldOf g2)
    , Pairing g1 g2 gt
    , KnownNat n
    , KnownNat (PlonkupPolyExtendedLength n)
    , UnivariateFieldPolyVec (ScalarFieldOf g2) pv
    , Bilinear (V.Vector g1) (pv (PlonkupPolyExtendedLength n)) g1
    ) => Plonkup i n l g1 g2 ts pv -> PlonkupSetup i n l g1 g2 pv
plonkupSetup Plonkup {..} =
    let gs = toV gs'
        h0 = pointGen

        relation@PlonkupRelation{..} = fromJust $ toPlonkupRelation ac :: PlonkupRelation i n l (ScalarFieldOf g1) pv

        f i = case (i-!1) `Prelude.div` value @n of
            0 -> omega^i
            1 -> k1 * (omega^i)
            2 -> k2 * (omega^i)
            _ -> error "setup: invalid index"

        s = f <$> fromPermutation @(PlonkupPermutationSize n) sigma
        sigma1s = toPolyVec $ V.take (fromIntegral $ value @n) s
        sigma2s = toPolyVec $ V.take (fromIntegral $ value @n) $ V.drop (fromIntegral $ value @n) s
        sigma3s = toPolyVec $ V.take (fromIntegral $ value @n) $ V.drop (fromIntegral $ 2 * value @n) s

        qmX = polyVecInLagrangeBasis omega qM
        qlX = polyVecInLagrangeBasis omega qL
        qrX = polyVecInLagrangeBasis omega qR
        qoX = polyVecInLagrangeBasis omega qO
        qcX = polyVecInLagrangeBasis omega qC
        qkX = polyVecInLagrangeBasis omega qK
        s1X = polyVecInLagrangeBasis omega sigma1s
        s2X = polyVecInLagrangeBasis omega sigma2s
        s3X = polyVecInLagrangeBasis omega sigma3s
        tX  = polyVecInLagrangeBasis omega t
        polynomials = PlonkupCircuitPolynomials {..}

        com = bilinear
        cmQl = gs `com` qlX
        cmQr = gs `com` qrX
        cmQo = gs `com` qoX
        cmQm = gs `com` qmX
        cmQc = gs `com` qcX
        cmQk = gs `com` qkX
        cmS1 = gs `com` s1X
        cmS2 = gs `com` s2X
        cmS3 = gs `com` s3X
        cmT1 = gs `com` tX
        commitments = PlonkupCircuitCommitments {..}

    in PlonkupSetup {..}
