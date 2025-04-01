{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}

module ZkFold.Base.Protocol.Plonkup.Setup where

import           Data.Binary                                       (Binary)
import           Data.Functor.Rep                                  (Rep, Representable)
import           Data.Maybe                                        (fromJust)
import qualified Data.Vector                                       as V
import           GHC.IsList                                        (IsList (..))
import           Prelude                                           hiding (Num (..), drop, length, sum, take, (!!), (/),
                                                                    (^))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number                  (KnownNat, value)
import           ZkFold.Base.Algebra.Basic.Permutations            (fromPermutation)
import           ZkFold.Base.Algebra.EllipticCurve.Class           (CyclicGroup (..), Pairing)
import           ZkFold.Base.Algebra.Polynomials.Univariate        (UnivariateFieldPolyVec (..), polyVecInLagrangeBasis,
                                                                    toPolyVec)
import           ZkFold.Base.Data.Vector                           (Vector (..))
import           ZkFold.Base.Protocol.Plonkup.Internal             (Plonkup (..), PlonkupPermutationSize,
                                                                    PlonkupPolyExtendedLength)
import           ZkFold.Base.Protocol.Plonkup.Prover.Polynomials   (PlonkupCircuitPolynomials (..))
import           ZkFold.Base.Protocol.Plonkup.Relation             (PlonkupRelation (..), toPlonkupRelation)
import           ZkFold.Base.Protocol.Plonkup.Verifier.Commitments (PlonkupCircuitCommitments (..))
import           ZkFold.Symbolic.Class                             (Arithmetic)

data PlonkupSetup p i n l g1 g2 pv = PlonkupSetup
    { omega       :: ScalarFieldOf g1
    , k1          :: ScalarFieldOf g1
    , k2          :: ScalarFieldOf g1
    , gs          :: V.Vector g1
    , h0          :: g2
    , h1          :: g2
    , sigma1s     :: pv n
    , sigma2s     :: pv n
    , sigma3s     :: pv n
    , relation    :: PlonkupRelation p i n l (ScalarFieldOf g1) pv
    , polynomials :: PlonkupCircuitPolynomials n g1 pv
    , commitments :: PlonkupCircuitCommitments g1
    }

instance
        ( Show g1
        , Show g2
        , Show (ScalarFieldOf g1)
        , Show (pv n)
        , Show (pv (PlonkupPolyExtendedLength n))
        , Show (PlonkupRelation p i n l (ScalarFieldOf g1) pv)
        ) => Show (PlonkupSetup p i n l g1 g2 pv) where
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

plonkupSetup :: forall i p n l g1 g2 gt ts pv .
    ( Representable p
    , Representable i
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
    ) => Plonkup p i n l g1 g2 ts pv -> PlonkupSetup p i n l g1 g2 pv
plonkupSetup Plonkup {..} =
    let gs = toV gs'
        h0 = pointGen

        relation@PlonkupRelation{..} = {-# SCC relation #-} fromJust $ toPlonkupRelation ac :: PlonkupRelation p i n l (ScalarFieldOf g1) pv

        f i = {-# SCC f #-} case (i-!1) `Prelude.div` value @n of
            0 -> omega^i
            1 -> k1 * (omega^i)
            2 -> k2 * (omega^i)
            _ -> error "setup: invalid index"

        g _ = omega

        s = {-# SCC s #-} f <$> fromPermutation @(PlonkupPermutationSize n) sigma
        sigma1s = {-# SCC sigma1s #-} toPolyVec $ V.take (fromIntegral $ value @n) s
        sigma2s = {-# SCC sigma2s #-} toPolyVec $ V.take (fromIntegral $ value @n) $ V.drop (fromIntegral $ value @n) s
        sigma3s = {-# SCC sigma3s #-} toPolyVec $ V.take (fromIntegral $ value @n) $ V.drop (fromIntegral $ 2 * value @n) s

        qmX = {-# SCC qmx #-} polyVecInLagrangeBasis omega qM
        qlX = {-# SCC qlx #-} polyVecInLagrangeBasis omega qL
        qrX = {-# SCC qrx #-} polyVecInLagrangeBasis omega qR
        qoX = {-# SCC qox #-} polyVecInLagrangeBasis omega qO
        qcX = {-# SCC qcx #-} polyVecInLagrangeBasis omega qC
        qkX = {-# SCC qkx #-} polyVecInLagrangeBasis omega qK
        s1X = {-# SCC s1x #-} polyVecInLagrangeBasis omega sigma1s
        s2X = {-# SCC s2x #-} polyVecInLagrangeBasis omega sigma2s
        s3X = {-# SCC s3x #-} polyVecInLagrangeBasis omega sigma3s
        tX  = {-# SCC tx  #-} polyVecInLagrangeBasis omega t
        polynomials = PlonkupCircuitPolynomials {..}

        com = bilinear
        cmQl = {-# SCC cmql #-} gs `com` qlX
        cmQr = {-# SCC cmqr #-} gs `com` qrX
        cmQo = {-# SCC cmqo #-} gs `com` qoX
        cmQm = {-# SCC cmqm #-} gs `com` qmX
        cmQc = {-# SCC cmqc #-} gs `com` qcX
        cmQk = {-# SCC cmqk #-} gs `com` qkX
        cmS1 = {-# SCC cms1 #-} gs `com` s1X
        cmS2 = {-# SCC cms2 #-} gs `com` s2X
        cmS3 = {-# SCC cms3 #-} gs `com` s3X
        cmT1 = {-# SCC cmt1 #-} gs `com` tX
        commitments = PlonkupCircuitCommitments {..}

    in PlonkupSetup {..}
