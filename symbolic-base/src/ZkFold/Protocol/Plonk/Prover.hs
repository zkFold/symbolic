{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Protocol.Plonk.Prover
    ( plonkProve
    ) where

import           Data.Bool                                  (bool)
import           Data.Foldable                              (length)
import qualified Data.Vector                                as V
import           Data.Word                                  (Word8)
import           GHC.IsList                                 (IsList (..))
import           Prelude                                    hiding (Num (..), drop, length, pi, replicate, sum, take,
                                                             (!!), (/), (^))

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.Class         (Compressible (..), CyclicGroup (..))
import           ZkFold.Algebra.Number                      (KnownNat, Natural, value)
import           ZkFold.Algebra.Polynomial.Univariate       hiding (qr)
import           ZkFold.Data.Vector                         ((!!))
import           ZkFold.Prelude                             (replicate)
import           ZkFold.Protocol.NonInteractiveProof
import           ZkFold.Protocol.Plonkup                    (with4n6)
import           ZkFold.Protocol.Plonkup.Input
import           ZkFold.Protocol.Plonkup.Internal           (PlonkupPolyExtended, PlonkupPolyExtendedLength)
import           ZkFold.Protocol.Plonkup.Proof
import           ZkFold.Protocol.Plonkup.Prover.Polynomials
import           ZkFold.Protocol.Plonkup.Prover.Secret
import           ZkFold.Protocol.Plonkup.Prover.Setup
import           ZkFold.Protocol.Plonkup.Relation           (PlonkupRelation (..))
import           ZkFold.Protocol.Plonkup.Testing            (PlonkupProverTestInfo (..))
import           ZkFold.Protocol.Plonkup.Utils              (sortByList)
import           ZkFold.Protocol.Plonkup.Witness

plonkProve :: forall i o n g1 g2 ts pv .
    ( Ord (ScalarFieldOf g1)
    , Scale (ScalarFieldOf g1) g1
    , Compressible g1
    , ToTranscript ts Word8
    , ToTranscript ts (ScalarFieldOf g1)
    , ToTranscript ts (Compressed g1)
    , FromTranscript ts (ScalarFieldOf g1)
    , Bilinear (V.Vector (ScalarFieldOf g1)) (pv (PlonkupPolyExtendedLength n)) (ScalarFieldOf g1)
    , KnownNat n
    , KnownNat (PlonkupPolyExtendedLength n)
    , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
    ) =>PlonkupProverSetup i o n g1 g2 pv -> (PlonkupWitnessInput i g1, PlonkupProverSecret g1) -> (PlonkupInput g1, PlonkupProof g1, PlonkupProverTestInfo n g1 pv)
plonkProve PlonkupProverSetup {..}
        (PlonkupWitnessInput wInput, PlonkupProverSecret ps)
    = (with4n6 @n $ PlonkupInput wPub, PlonkupProof {..}, PlonkupProverTestInfo {..})
    where

        PlonkupCircuitPolynomials {..} = polynomials
        secret i = ps !! (i -! 1)

        n = value @n
        zhX = with4n6 @n $ polyVecZero (value @n)

        (w1, w2, w3) = witness relation wInput
        wPub = pubInput relation wInput

        w1X = with4n6 @n $ polyVecInLagrangeBasis omega w1 :: PlonkupPolyExtended n g1 pv
        w2X = with4n6 @n $ polyVecInLagrangeBasis omega w2 :: PlonkupPolyExtended n g1 pv
        w3X = with4n6 @n $ polyVecInLagrangeBasis omega w3 :: PlonkupPolyExtended n g1 pv

        pi  = toPolyVec $ fromList $ replicate (prvNum relation) zero ++ map negate wPub :: pv n
        piX = with4n6 @n $ polyVecInLagrangeBasis omega pi  :: PlonkupPolyExtended n g1 pv

        -- Round 1

        aX = with4n6 @n $ polyVecLinear (secret 1) (secret 2) * zhX + w1X :: PlonkupPolyExtended n g1 pv
        bX = with4n6 @n $ polyVecLinear (secret 3) (secret 4) * zhX + w2X :: PlonkupPolyExtended n g1 pv
        cX = with4n6 @n $ polyVecLinear (secret 5) (secret 6) * zhX + w3X :: PlonkupPolyExtended n g1 pv

        com = bilinear
        cmA = (gs `com` aX) `scale` g1 
        cmB = (gs `com` bX) `scale` g1
        cmC = (gs `com` cX) `scale` g1

        -- Round 2

        ts1   = mempty
            `transcript` compress cmA
            `transcript` compress cmB
            `transcript` compress cmC :: ts
        zeta = challenge ts1 :: ScalarFieldOf g1

        f_zeta' = w1 + zeta *. (w2 + zeta *. w3)
        t_zeta = t1 relation + zeta *. (t2 relation + zeta *. t3 relation)
        f_zeta = toPolyVec $ V.zipWith3 (\lk ti ai -> bool ti ai (lk == one)) (fromPolyVec $ qK relation) (fromPolyVec t_zeta) (fromPolyVec f_zeta') :: pv n

        fX = with4n6 @n $ polyVecLinear (secret 7) (secret 8) * zhX + polyVecInLagrangeBasis omega f_zeta :: PlonkupPolyExtended n g1 pv
        tX = t1X

        s  = sortByList (V.toList (fromPolyVec f_zeta) ++ V.toList (fromPolyVec t_zeta)) (V.toList $ fromPolyVec t_zeta)
        h1 = toPolyVec $ V.ifilter (\i _ -> odd i) $ fromList s  :: pv n
        h2 = toPolyVec $ V.ifilter (\i _ -> even i) $ fromList s :: pv n

        h1X = with4n6 @n $ polyVecQuadratic (secret 9) (secret 10) (secret 11) * zhX + polyVecInLagrangeBasis omega h1 :: PlonkupPolyExtended n g1 pv
        h2X = with4n6 @n $ polyVecLinear (secret 12) (secret 13) * zhX + polyVecInLagrangeBasis omega h2 :: PlonkupPolyExtended n g1 pv

        cmF  = (gs `com` fX ) `scale` g1 
        cmH1 = (gs `com` h1X) `scale` g1
        cmH2 = (gs `com` h2X) `scale` g1

        -- Round 3

        ts2 = ts1
            -- `transcript` compress cmF
            -- `transcript` compress cmH1
            -- `transcript` compress cmH2
        beta    = challenge (ts2 `transcript` (1 :: Word8))
        gamma   = challenge (ts2 `transcript` (2 :: Word8))
        delta   = challenge (ts2 `transcript` (3 :: Word8))
        epsilon = challenge (ts2 `transcript` (4 :: Word8))

        omegas  = toPolyVec $ V.iterateN (fromIntegral n) (* omega) omega
        omegas' = with4n6 @n $ toPolyVec $ V.iterateN (fromIntegral $ value @(PlonkupPolyExtendedLength n)) (* omega) one

        cumprod :: pv n -> pv n
        cumprod = toPolyVec . V.scanl1' (*) . fromPolyVec

        rotR :: pv n -> pv n
        rotR p = toPolyVec $ V.drop (fromIntegral $ value @n -! 1) (fromPolyVec p) V.++ V.take (fromIntegral $ value @n -! 1) (fromPolyVec p)

        rotL :: pv n -> pv n
        rotL p = toPolyVec $ V.drop 1 (fromPolyVec p) V.++ V.take 1 (fromPolyVec p)

        -- TODO: check operation order
        grandProduct1 = rotR . cumprod $
                (w1 + (beta *. omegas) .+ gamma)
            .*. (w2 + ((beta * k1) *. omegas) .+ gamma)
            .*. (w3 + ((beta * k2) *. omegas) .+ gamma)
            ./. (w1 + (beta *. sigma1s) .+ gamma)
            ./. (w2 + (beta *. sigma2s) .+ gamma)
            ./. (w3 + (beta *. sigma3s) .+ gamma)
        z1X = with4n6 @n $ polyVecQuadratic (secret 14) (secret 15) (secret 16) * zhX + polyVecInLagrangeBasis omega grandProduct1 :: PlonkupPolyExtended n g1 pv

        grandProduct2 = rotR . cumprod $
                (one + delta) *. (epsilon +. f_zeta)
            .*. ((epsilon * (one + delta)) +. t_zeta + delta *. rotL t_zeta)
            ./. ((epsilon * (one + delta)) +. h1 + delta *. h2)
            ./. ((epsilon * (one + delta)) +. h2 + delta *. rotL h1)
        z2X = with4n6 @n $ polyVecQuadratic (secret 17) (secret 18) (secret 19) * zhX + polyVecInLagrangeBasis omega grandProduct2 :: PlonkupPolyExtended n g1 pv

        cmZ1 = (gs `com` z1X) `scale` g1 
        cmZ2 = (gs `com` z2X) `scale` g1

        -- Round 4

        ts3 = ts2
            `transcript` compress cmZ1
            -- `transcript` compress cmZ2
        alpha  = challenge ts3
        alpha2 = alpha * alpha
        -- alpha3 = alpha2 * alpha
        -- alpha4 = alpha3 * alpha
        -- alpha5 = alpha4 * alpha

        gammaX   = polyVecConstant gamma
        -- deltaX   = scalePV delta one
        -- epsilonX = scalePV epsilon one
        qX = with4n6 @n $ (
                (qmX * aX * bX + qlX * aX + qrX * bX + qoX * cX + piX + qcX)
              + (aX + polyVecLinear beta gamma) * (bX + polyVecLinear (beta * k1) gamma) * (cX + polyVecLinear (beta * k2) gamma) * z1X .* alpha
              - (aX + (beta *. s1X) + gammaX) * (bX + (beta *. s2X) + gammaX) * (cX + (beta *. s3X) + gammaX) * (z1X .*. omegas') .* alpha
              + (z1X - one) * polyVecLagrange (value @n) 1 omega .* alpha2
            --   + qkX * (aX - fX) .* alpha3
            --   + z2X * (one + deltaX) * (epsilonX + fX) * ((epsilonX * (one + deltaX)) + tX + deltaX * (tX .*. omegas')) .* alpha4
            --   - (z2X .*. omegas') * ((epsilonX * (one + deltaX)) + h1X + deltaX * h2X) * ((epsilonX * (one + deltaX)) + h2X + deltaX * (h1X .*. omegas')) .* alpha4
            --   + (z2X - one) * polyVecLagrange @_ @n 1 omega .* alpha5
            ) `polyVecDiv` zhX
        qlowX  = with4n6 @n $ toPolyVec $ V.take (fromIntegral (n+2)) $ fromPolyVec qX
        qmidX  = with4n6 @n $ toPolyVec $ V.take (fromIntegral (n+2)) $ V.drop (fromIntegral (n+2)) $ fromPolyVec qX
        qhighX = with4n6 @n $ toPolyVec $ V.drop (fromIntegral (2*(n+2))) $ fromPolyVec qX

        cmQlow  = (gs `com` qlowX)  `scale` g1 
        cmQmid  = (gs `com` qmidX)  `scale` g1
        cmQhigh = (gs `com` qhighX) `scale` g1

        -- Round 5

        ts4 = ts3
            `transcript` compress cmQlow
            `transcript` compress cmQmid
            `transcript` compress cmQhigh
        xi = challenge ts4

        a_xi    = aX `evalPolyVec` xi
        b_xi    = bX `evalPolyVec` xi
        c_xi    = cX `evalPolyVec` xi
        s1_xi   = s1X `evalPolyVec` xi
        s2_xi   = s2X `evalPolyVec` xi
        f_xi    = fX `evalPolyVec` xi
        t_xi    = tX `evalPolyVec` xi
        t_xi'   = tX `evalPolyVec` (xi * omega)
        z1_xi'  = z1X `evalPolyVec` (xi * omega)
        z2_xi'  = z2X `evalPolyVec` (xi * omega)
        h1_xi'  = h1X `evalPolyVec` (xi * omega)
        h2_xi   = h2X `evalPolyVec` xi
        lag1_xi = polyVecLagrange @_ @pv @(PlonkupPolyExtendedLength n) (value @n) 1 omega `evalPolyVec` xi
        l1_xi   = one // (scale n one * (xi - omega))
        l_xi    = map (\i -> one // (scale n one * (xi - omega^i))) [prvNum relation + 1 :: Natural .. fromIntegral (length wPub)]

        -- Round 6

        ts5 = ts4
            `transcript` a_xi
            `transcript` b_xi
            `transcript` c_xi
            `transcript` s1_xi
            `transcript` s2_xi
            -- `transcript` f_xi
            -- `transcript` t_xi
            -- `transcript` t_xi'
            `transcript` z1_xi'
            -- `transcript` z2_xi'
            -- `transcript` h1_xi'
            -- `transcript` h2_xi
        v = challenge ts5

        pi_xi = piX `evalPolyVec` xi
        zhX_xi = zhX `evalPolyVec` xi

        rX = with4n6 @n $
                qmX .* (a_xi * b_xi) + qlX .* a_xi + qrX .* b_xi + qoX .* c_xi + one .* pi_xi + qcX
              + alpha *. (((a_xi + beta * xi + gamma) * (b_xi + beta * k1 * xi + gamma) * (c_xi + beta * k2 * xi + gamma)) *. z1X
                        - ((a_xi + beta * s1_xi + gamma) * (b_xi + beta * s2_xi + gamma) * z1_xi') *. (one .* c_xi + beta *. s3X + one .* gamma)
                    )
              + (alpha2 * lag1_xi) *. (z1X - one)
            --   + (alpha3 * (a_xi - f_xi)) *. qkX
            --   + alpha4 *. (((one + delta) * (epsilon + f_xi) * ((epsilon * (one + delta)) + t_xi + delta * t_xi')) *. z2X
            --             - (z2_xi' * ((epsilon * (one + delta)) + h2_xi + delta * h1_xi')) *. (one .* (epsilon * (one + delta)) + h1X + one .* (delta * h2_xi))
            --         )
            --   + (alpha5 * lag1_xi) *. (z2X - one)
              - zhX_xi *. (qlowX + (xi^(n+2)) *. qmidX + (xi^(2*n+4)) *. qhighX)

        vn i = v ^ (i :: Natural)

        proofX1 = with4n6 @n $ (
                  rX
                + (vn 1 *. (aX - (a_xi *. one)))
                + (vn 2 *. (bX - (b_xi *. one)))
                + (vn 3 *. (cX - (c_xi *. one)))
                + (vn 4 *. (s1X - (s1_xi *. one)))
                + (vn 5 *. (s2X - (s2_xi *. one)))
                -- + (vn 6 *. (fX - (f_xi *. one)))
                -- + (vn 7 *. (tX - (t_xi *. one)))
                -- + (vn 8 *. (h2X - (h2_xi *. one)))
            ) `polyVecDiv` polyVecLinear one (negate xi)
        proofX2 = with4n6 @n $ (
                  z1X - (z1_xi' *. one)
                -- + (vn 1 *. (tX - (t_xi' *. one)))
                -- + (vn 2 *. (z2X - (z2_xi' *. one)))
                -- + (vn 3 *. (h1X - (h1_xi' *. one)))
            ) `polyVecDiv` polyVecLinear one (negate (xi * omega))

        proof1 = (gs `com` proofX1) `scale` g1 
        proof2 = (gs `com` proofX2) `scale` g1
