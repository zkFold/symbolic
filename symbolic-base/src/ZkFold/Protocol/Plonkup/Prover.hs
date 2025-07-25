{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Protocol.Plonkup.Prover (
  module ZkFold.Protocol.Plonkup.Prover.Polynomials,
  module ZkFold.Protocol.Plonkup.Prover.Secret,
  module ZkFold.Protocol.Plonkup.Prover.Setup,
  plonkupProve,
) where

import Data.Bool (bool)
import Data.Foldable (length)
import qualified Data.Vector as V
import Data.Word (Word8)
import GHC.IsList (IsList (..))
import Prelude hiding (
  Num (..),
  drop,
  length,
  pi,
  replicate,
  sum,
  take,
  (!!),
  (/),
  (^),
 )

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (Compressible (..), CyclicGroup (..))
import ZkFold.Algebra.Number (KnownNat, Natural, value)
import ZkFold.Algebra.Polynomial.Univariate hiding (qr)
import ZkFold.Data.Vector ((!!))
import ZkFold.FFI.Rust.Conversion
import ZkFold.FFI.Rust.Poly ()
import ZkFold.FFI.Rust.RustBLS ()
import ZkFold.Prelude (replicate)
import ZkFold.Protocol.NonInteractiveProof
import ZkFold.Protocol.Plonkup.Input
import ZkFold.Protocol.Plonkup.Internal (PlonkupPolyExtended, PlonkupPolyExtendedLength)
import ZkFold.Protocol.Plonkup.Proof
import ZkFold.Protocol.Plonkup.Prover.Polynomials
import ZkFold.Protocol.Plonkup.Prover.Secret
import ZkFold.Protocol.Plonkup.Prover.Setup
import ZkFold.Protocol.Plonkup.Relation (PlonkupRelation (..))
import ZkFold.Protocol.Plonkup.Testing (PlonkupProverTestInfo (..))
import ZkFold.Protocol.Plonkup.Utils (sortByList)
import ZkFold.Protocol.Plonkup.Witness

plonkupProve
  :: forall i o n g1 g2 ts pv rustG1 rustPv
   . ( Ord (ScalarFieldOf g1)
     , Compressible g1
     , ToTranscript ts Word8
     , ToTranscript ts (ScalarFieldOf g1)
     , ToTranscript ts (Compressed g1)
     , FromTranscript ts (ScalarFieldOf g1)
     , KnownNat n
     , KnownNat (PlonkupPolyExtendedLength n)
     , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
     , Bilinear (V.Vector rustG1) (pv (PlonkupPolyExtendedLength n)) g1
     , UnivariateFieldPolyVec (ScalarFieldOf rustG1) rustPv
     , RustHaskell rustG1 g1
     , RustHaskell (ScalarFieldOf rustG1) (ScalarFieldOf g1)
     , RustHaskell (rustPv (PlonkupPolyExtendedLength n)) (pv (PlonkupPolyExtendedLength n))
     )
  => PlonkupProverSetup i o n g1 g2 pv
  -> (PlonkupWitnessInput i g1, PlonkupProverSecret g1)
  -> (PlonkupInput g1, PlonkupProof g1, PlonkupProverTestInfo n g1 pv)
plonkupProve
  PlonkupProverSetup {..}
  (PlonkupWitnessInput wInput, PlonkupProverSecret ps) =
    (PlonkupInput wPub, PlonkupProof {..}, PlonkupProverTestInfo {..})
   where
    PlonkupCircuitPolynomials {..} = polynomials
    secret i = ps !! (i -! 1)

    !n = value @n
    !zhX = polyVecZero (value @n)

    !gsR = h2r <$> gs

    (!w1, !w2, !w3) = witness relation wInput
    !wPub = pubInput relation wInput

    !w1X = polyVecInLagrangeBasis omega w1 :: PlonkupPolyExtended n g1 pv
    !w2X = polyVecInLagrangeBasis omega w2 :: PlonkupPolyExtended n g1 pv
    !w3X = polyVecInLagrangeBasis omega w3 :: PlonkupPolyExtended n g1 pv

    -- Extending public input to the polynomial domain
    !pi = toPolyVec $ fromList $ replicate (prvNum relation) zero ++ map negate wPub :: pv n
    !piX = polyVecInLagrangeBasis omega pi :: PlonkupPolyExtended n g1 pv

    -- Round 1

    !aX = polyVecLinear (secret 1) (secret 2) * zhX + w1X :: PlonkupPolyExtended n g1 pv
    !bX = polyVecLinear (secret 3) (secret 4) * zhX + w2X :: PlonkupPolyExtended n g1 pv
    !cX = polyVecLinear (secret 5) (secret 6) * zhX + w3X :: PlonkupPolyExtended n g1 pv

    !com = bilinear
    !cmA = gsR `com` aX
    !cmB = gsR `com` bX
    !cmC = gsR `com` cX

    -- Round 2

    !ts1 =
      mempty
        `transcript` compress cmA
        `transcript` compress cmB
        `transcript` compress cmC
        :: ts
    zeta = challenge ts1 :: ScalarFieldOf g1

    !f_zeta' = w1 + zeta *. (w2 + zeta *. w3)
    !t_zeta = t1 relation + zeta *. (t2 relation + zeta *. t3 relation)
    !f_zeta =
      toPolyVec $
        V.zipWith3 (\lk ti ai -> bool ti ai (lk == one)) (fromPolyVec $ qK relation) (fromPolyVec t_zeta) (fromPolyVec f_zeta')
        :: pv n

    !fX = polyVecLinear (secret 7) (secret 8) * zhX + polyVecInLagrangeBasis omega f_zeta :: PlonkupPolyExtended n g1 pv
    !tX = t1X + zeta *. (t2X + zeta *. t3X) :: PlonkupPolyExtended n g1 pv

    !s = sortByList (V.toList $ fromPolyVec f_zeta V.++ fromPolyVec t_zeta) (V.toList $ fromPolyVec t_zeta)
    -- In the paper, vectors are indexed from 1, but in Haskell from 0, so h1 contains even indices and h2 odd
    !h1 = toPolyVec $ V.ifilter (\i _ -> even i) $ fromList s :: pv n
    !h2 = toPolyVec $ V.ifilter (\i _ -> odd i) $ fromList s :: pv n

    !h1X =
      polyVecQuadratic (secret 9) (secret 10) (secret 11) * zhX + polyVecInLagrangeBasis omega h1
        :: PlonkupPolyExtended n g1 pv
    !h2X = polyVecLinear (secret 12) (secret 13) * zhX + polyVecInLagrangeBasis omega h2 :: PlonkupPolyExtended n g1 pv

    !cmF = gsR `com` fX
    !cmH1 = gsR `com` h1X
    !cmH2 = gsR `com` h2X

    -- Round 3

    !ts2 =
      ts1
        `transcript` compress cmF
        `transcript` compress cmH1
        `transcript` compress cmH2
    !beta = challenge (ts2 `transcript` (1 :: Word8))
    !gamma = challenge (ts2 `transcript` (2 :: Word8))
    !delta = challenge (ts2 `transcript` (3 :: Word8))
    !epsilon = challenge (ts2 `transcript` (4 :: Word8))

    !omegas = toPolyVec $ V.iterateN (fromIntegral n) (* omega) omega
    !omegas' = toPolyVec $ V.iterateN (fromIntegral $ value @(PlonkupPolyExtendedLength n)) (* omega) one

    cumprod :: pv n -> pv n
    cumprod = toPolyVec . V.scanl1' (*) . fromPolyVec

    rotR :: pv n -> pv n
    rotR p =
      toPolyVec $
        V.drop (fromIntegral $ value @n -! 1) (fromPolyVec p) V.++ V.take (fromIntegral $ value @n -! 1) (fromPolyVec p)

    rotL :: pv n -> pv n
    rotL p = toPolyVec $ V.drop 1 (fromPolyVec p) V.++ V.take 1 (fromPolyVec p)

    -- TODO: check operation order
    !grandProduct1 =
      rotR . cumprod $
        (w1 + (beta *. omegas) .+ gamma)
          .*. (w2 + ((beta * k1) *. omegas) .+ gamma)
          .*. (w3 + ((beta * k2) *. omegas) .+ gamma)
          ./. (w1 + (beta *. sigma1s) .+ gamma)
          ./. (w2 + (beta *. sigma2s) .+ gamma)
          ./. (w3 + (beta *. sigma3s) .+ gamma)
    !z1X =
      polyVecQuadratic (secret 14) (secret 15) (secret 16) * zhX + polyVecInLagrangeBasis omega grandProduct1
        :: PlonkupPolyExtended n g1 pv

    !grandProduct2 =
      rotR . cumprod $
        (one + delta)
          *. (epsilon +. f_zeta)
          .*. ((epsilon * (one + delta)) +. t_zeta + delta *. rotL t_zeta)
          ./. ((epsilon * (one + delta)) +. h1 + delta *. h2)
          ./. ((epsilon * (one + delta)) +. h2 + delta *. rotL h1)
    !z2X =
      polyVecQuadratic (secret 17) (secret 18) (secret 19) * zhX + polyVecInLagrangeBasis omega grandProduct2
        :: PlonkupPolyExtended n g1 pv

    !cmZ1 = gsR `com` z1X
    !cmZ2 = gsR `com` z2X

    -- Round 4

    !ts3 =
      ts2
        `transcript` compress cmZ1
        `transcript` compress cmZ2
    !alpha = challenge ts3
    !alpha2 = alpha * alpha
    !alpha3 = alpha2 * alpha
    !alpha4 = alpha3 * alpha
    !alpha5 = alpha4 * alpha

    !gammaX = polyVecConstant gamma
    !deltaX = polyVecConstant delta
    !epsilonX = polyVecConstant epsilon

    qmXR :: rustPv (PlonkupPolyExtendedLength n)
    !qmXR = h2r qmX
    !qlXR = h2r qlX
    !qrXR = h2r qrX
    !qoXR = h2r qoX
    !qcXR = h2r qcX
    !qkXR = h2r qkX

    !piXR = h2r piX

    !aXR = h2r aX
    !bXR = h2r bX
    !cXR = h2r cX

    !z1XR = h2r z1X
    !z2XR = h2r z2X

    !s1XR = h2r s1X
    !s2XR = h2r s2X
    !s3XR = h2r s3X

    !h1XR = h2r h1X
    !h2XR = h2r h2X

    !fXR = h2r fX
    !tXR = h2r tX

    alphaR :: ScalarFieldOf rustG1
    !alphaR = h2r alpha
    !alpha2R = h2r alpha2
    !alpha3R = h2r alpha3
    !alpha4R = h2r alpha4
    !alpha5R = h2r alpha5
    !betaR = h2r beta
    !gammaXR = h2r gammaX
    !deltaXR = h2r deltaX
    !epsilonXR = h2r epsilonX
    !zetaR = h2r zeta

    !omegas'R = h2r omegas'

    polyVecLinearR :: ScalarFieldOf g1 -> ScalarFieldOf g1 -> rustPv (PlonkupPolyExtendedLength n)
    polyVecLinearR a b = h2r $ polyVecLinear a b

    !qXs1 = (qmXR * aXR * bXR + qlXR * aXR + qrXR * bXR + qoXR * cXR + piXR + qcXR)
    !qXs2 =
      ( (aXR + polyVecLinearR beta gamma)
          * (bXR + polyVecLinearR (beta * k1) gamma)
          * (cXR + polyVecLinearR (beta * k2) gamma)
          * z1XR .* alphaR
      )
    !qXs3 =
      ( (aXR + (betaR *. s1XR) + gammaXR)
          * (bXR + (betaR *. s2XR) + gammaXR)
          * (cXR + (betaR *. s3XR) + gammaXR)
          * (z1XR .*. omegas'R) .* alphaR
      )
    !qXs4 = ((z1XR - one) * h2r (polyVecLagrange (value @n) 1 omega) .* alpha2R)
    !qXs5 = (qkXR * (aXR + zetaR *. (bXR + zetaR *. cXR) - fXR) .* alpha3R)
    !qXs6 =
      ( z2XR
          * (one + deltaXR)
          * (epsilonXR + fXR)
          * ((epsilonXR * (one + deltaXR)) + tXR + deltaXR * (tXR .*. omegas'R)) .* alpha4R
      )
    !qXs7 =
      ( (z2XR .*. omegas'R)
          * ((epsilonXR * (one + deltaXR)) + h1XR + deltaXR * h2XR)
          * ((epsilonXR * (one + deltaXR)) + h2XR + deltaXR * (h1XR .*. omegas'R)) .* alpha4R
      )

    !qXs8 = ((z2XR - one) * h2r (polyVecLagrange (value @n) 1 omega) .* alpha5R)

    !qXNumerator =
      ( qXs1
          + qXs2
          - qXs3
          + qXs4
          + qXs5
          + qXs6
          - qXs7
          + qXs8
      )

    !qX = r2h qXNumerator `polyVecDiv` zhX

    !qlowX = toPolyVec $ V.take (fromIntegral (n + 2)) $ fromPolyVec qX
    !qmidX = toPolyVec $ V.take (fromIntegral (n + 2)) $ V.drop (fromIntegral (n + 2)) $ fromPolyVec qX
    !qhighX = toPolyVec $ V.drop (fromIntegral (2 * (n + 2))) $ fromPolyVec qX

    !cmQlow = gsR `com` qlowX
    !cmQmid = gsR `com` qmidX
    !cmQhigh = gsR `com` qhighX

    -- Round 5

    !ts4 =
      ts3
        `transcript` compress cmQlow
        `transcript` compress cmQmid
        `transcript` compress cmQhigh
    !xi = challenge ts4

    !a_xi = aX `evalPolyVec` xi
    !b_xi = bX `evalPolyVec` xi
    !c_xi = cX `evalPolyVec` xi
    !s1_xi = s1X `evalPolyVec` xi
    !s2_xi = s2X `evalPolyVec` xi
    !f_xi = fX `evalPolyVec` xi
    !t_xi = tX `evalPolyVec` xi
    !t_xi' = tX `evalPolyVec` (xi * omega)
    !z1_xi' = z1X `evalPolyVec` (xi * omega)
    !z2_xi' = z2X `evalPolyVec` (xi * omega)
    !h1_xi' = h1X `evalPolyVec` (xi * omega)
    !h2_xi = h2X `evalPolyVec` xi
    !lag1_xi = polyVecLagrange @_ @pv @(PlonkupPolyExtendedLength n) (value @n) 1 omega `evalPolyVec` xi
    !l1_xi = finv (scale n (xi - omega))
    !l_xi = map (\i -> finv (scale n (xi - omega ^ i))) [prvNum relation + 1 :: Natural .. fromIntegral (length wPub)]

    -- Round 6

    !ts5 =
      ts4
        `transcript` a_xi
        `transcript` b_xi
        `transcript` c_xi
        `transcript` s1_xi
        `transcript` s2_xi
        `transcript` f_xi
        `transcript` t_xi
        `transcript` t_xi'
        `transcript` z1_xi'
        `transcript` z2_xi'
        `transcript` h1_xi'
        `transcript` h2_xi
    !v = challenge ts5

    !pi_xi = piX `evalPolyVec` xi
    !zhX_xi = zhX `evalPolyVec` xi

    !rX =
      qmX .* (a_xi * b_xi)
        + qlX .* a_xi
        + qrX .* b_xi
        + qoX .* c_xi
        + one .* pi_xi
        + qcX
        + alpha
          *. ( ((a_xi + beta * xi + gamma) * (b_xi + beta * k1 * xi + gamma) * (c_xi + beta * k2 * xi + gamma)) *. z1X
                 - ((a_xi + beta * s1_xi + gamma) * (b_xi + beta * s2_xi + gamma) * z1_xi') *. (one .* c_xi + beta *. s3X + one .* gamma)
             )
        + (alpha2 * lag1_xi) *. (z1X - one)
        + (alpha3 * (a_xi + zeta * (b_xi + zeta * c_xi) - f_xi)) *. qkX
        + alpha4
          *. ( ((one + delta) * (epsilon + f_xi) * ((epsilon * (one + delta)) + t_xi + delta * t_xi')) *. z2X
                 - (z2_xi' * ((epsilon * (one + delta)) + h2_xi + delta * h1_xi'))
                   *. (one .* (epsilon * (one + delta)) + h1X + one .* (delta * h2_xi))
             )
        + (alpha5 * lag1_xi) *. (z2X - one)
        - zhX_xi *. (qlowX + (xi ^ (n + 2)) *. qmidX + (xi ^ (2 * n + 4)) *. qhighX)

    vn :: Natural -> ScalarFieldOf g1
    vn i = v ^ i

    !proofX1 =
      ( rX
          + (vn 1 *. (aX - (a_xi *. one)))
          + (vn 2 *. (bX - (b_xi *. one)))
          + (vn 3 *. (cX - (c_xi *. one)))
          + (vn 4 *. (s1X - (s1_xi *. one)))
          + (vn 5 *. (s2X - (s2_xi *. one)))
          + (vn 6 *. (fX - (f_xi *. one)))
          + (vn 7 *. (tX - (t_xi *. one)))
          + (vn 8 *. (h2X - (h2_xi *. one)))
      )
        `polyVecDiv` polyVecLinear one (negate xi)
    !proofX2 =
      ( z1X
          - (z1_xi' *. one)
          + (vn 1 *. (tX - (t_xi' *. one)))
          + (vn 2 *. (z2X - (z2_xi' *. one)))
          + (vn 3 *. (h1X - (h1_xi' *. one)))
      )
        `polyVecDiv` polyVecLinear one (negate (xi * omega))

    !proof1 = gsR `com` proofX1
    !proof2 = gsR `com` proofX2
