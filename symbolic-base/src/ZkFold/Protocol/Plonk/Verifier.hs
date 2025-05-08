{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Protocol.Plonk.Verifier
    ( plonkVerify
    ) where

import           Data.Word                                    (Word8)
import           GHC.IsList                                   (IsList (..))
import           Prelude                                      hiding (Num (..), Ord, drop, length, sum, take, (!!), (/),
                                                               (^))

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.Class
import           ZkFold.Algebra.Number                        (KnownNat, Natural, value)
import           ZkFold.Algebra.Polynomial.Univariate         hiding (qr)
import           ZkFold.Protocol.NonInteractiveProof          hiding (verify)
import           ZkFold.Protocol.Plonkup.Input
import           ZkFold.Protocol.Plonkup.Internal
import           ZkFold.Protocol.Plonkup.Proof
import           ZkFold.Protocol.Plonkup.Verifier.Commitments
import           ZkFold.Protocol.Plonkup.Verifier.Setup

plonkVerify :: forall i o n g1 g2 gt ts pv .
    ( Pairing g1 g2 gt
    , Compressible g1
    , Eq gt
    , ToTranscript ts Word8
    , ToTranscript ts (ScalarFieldOf g1)
    , ToTranscript ts (Compressed g1)
    , FromTranscript ts (ScalarFieldOf g1)
    , KnownNat n
    , KnownNat (PlonkupPolyExtendedLength n)
    , UnivariateFieldPolyVec (ScalarFieldOf g2) pv
    ) =>PlonkupVerifierSetup i o n g1 g2 pv -> PlonkupInput g1 -> PlonkupProof g1 -> Bool
plonkVerify
    PlonkupVerifierSetup {..}
    (PlonkupInput wPub)
    (PlonkupProof {..}) = p1 == p2
    where
        PlonkupCircuitCommitments {..} = commitments

        n = value @n

        -- Step 4: Compute challenges

        ts1   = mempty
            `transcript` compress cmA
            `transcript` compress cmB
            `transcript` compress cmC :: ts

        ts2 = ts1
            -- `transcript` compress cmF
            -- `transcript` compress cmH1
            -- `transcript` compress cmH2
        beta    = challenge (ts2 `transcript` (1 :: Word8))
        gamma   = challenge (ts2 `transcript` (2 :: Word8))
        -- delta   = challenge (ts2 `transcript` (3 :: Word8))
        -- epsilon = challenge (ts2 `transcript` (4 :: Word8))

        ts3 = ts2
            `transcript` compress cmZ1
            -- `transcript` compress cmZ2
        alpha  = challenge ts3
        alpha2 = alpha * alpha
        -- alpha3 = alpha2 * alpha
        -- alpha4 = alpha3 * alpha
        -- alpha5 = alpha4 * alpha

        ts4 = ts3
            `transcript` compress cmQlow
            `transcript` compress cmQmid
            `transcript` compress cmQhigh
        xi = challenge ts4

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
        vn i = v ^ (i :: Natural)

        ts6 = ts5
            `transcript` compress proof1
            `transcript` compress proof2
        eta = challenge ts6

        -- Step 5: Compute zero polynomial evaluation
        zhX_xi = with4n6 @n $ polyVecZero @(ScalarFieldOf g1) @pv @(PlonkupPolyExtendedLength n) (value @n) `evalPolyVec` xi :: ScalarFieldOf g1

        -- Step 6: Compute Lagrange polynomial evaluation
        lagrange1_xi = with4n6 @n $ polyVecLagrange @(ScalarFieldOf g1) @pv @(PlonkupPolyExtendedLength n) (value @n) 1 omega `evalPolyVec` xi

        -- Step 7: Compute public polynomial evaluation
        pi_xi = with4n6 @n $ polyVecInLagrangeBasis @(ScalarFieldOf g1) @pv @_ @(PlonkupPolyExtendedLength n) omega
            (toPolyVec $ fromList $ foldMap (\x -> [negate x]) wPub :: pv n)
            `evalPolyVec` xi

        -- Step 8: Compute the public table commitment
        -- cmT_zeta = cmT1

        -- Step 9: Compute r0
        r0 = pi_xi
            - alpha * (a_xi + beta * s1_xi + gamma) * (b_xi + beta * s2_xi + gamma) * (c_xi + gamma) * z1_xi'
            - alpha2 * lagrange1_xi
            -- - alpha4 * z2_xi' * (epsilon * (one + delta) + delta * h2_xi) * (epsilon * (one + delta) + h2_xi + delta * h1_xi')
            -- - alpha5 * lagrange1_xi

        -- Step 10: Compute D
        d =
                (a_xi * b_xi) `scale` cmQm + a_xi `scale` cmQl + b_xi `scale` cmQr + c_xi `scale` cmQo + cmQc
              + ((a_xi + beta * xi + gamma) * (b_xi + beta * k1 * xi + gamma) * (c_xi + beta * k2 * xi + gamma) * alpha + lagrange1_xi * alpha2) `scale` cmZ1
              - ((a_xi + beta * s1_xi + gamma) * (b_xi + beta * s2_xi + gamma) * alpha * beta * z1_xi') `scale` cmS3
            --   + ((a_xi - f_xi) * alpha3) `scale` cmQk
            --   + ((one + delta) * (epsilon + f_xi) * (epsilon * (one + delta) + t_xi + delta * t_xi') * alpha4 + lagrange1_xi * alpha5) `scale` cmZ2
            --   - (z2_xi' * (epsilon * (one + delta) + h2_xi + delta * h1_xi') * alpha4) `scale` cmH1
              - zhX_xi `scale` (cmQlow + (xi^(n+2)) `scale` cmQmid + (xi^(2*n+4)) `scale` cmQhigh)

        -- Step 11: Compute F
        f = d
            + v `scale` cmA
            + vn 2 `scale` cmB
            + vn 3 `scale` cmC
            + vn 4 `scale` cmS1
            + vn 5 `scale` cmS2
            -- + vn 6 `scale` cmF
            -- + vn 7 `scale` cmT_zeta
            -- + vn 8 `scale` cmH2
            + eta `scale` cmZ1
            -- + (eta * v) `scale` cmT_zeta
            -- + (eta * vn 2) `scale` cmZ2
            -- + (eta * vn 3) `scale` cmH1

        -- Step 12: Compute E
        e = (
                negate r0 + v * a_xi + vn 2 * b_xi + vn 3 * c_xi + vn 4 * s1_xi + vn 5 * s2_xi + eta * z1_xi'
            ) `scale` pointGen

        -- Step 13: Compute the pairing
        p1 = pairing (proof1 + eta `scale` proof2) h1
        p2 = pairing (xi `scale` proof1 + (eta * xi * omega) `scale` proof2 + f - e) (pointGen @g2)
