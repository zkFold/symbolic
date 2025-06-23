{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Setup where

import Data.Binary (Binary)
import Data.Functor.Rep (Rep, Representable)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Prelude hiding (Num (..), drop, length, sum, take, (!!), (/), (^))

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..), Pairing)
import ZkFold.Algebra.Number (KnownNat, value)
import ZkFold.Algebra.Permutation (fromPermutation)
import ZkFold.Algebra.Polynomial.Univariate (
  UnivariateFieldPolyVec (..),
  polyVecInLagrangeBasis,
  toPolyVec,
 )
import ZkFold.Data.Vector (Vector (..))
import ZkFold.FFI.Rust.Conversion
import ZkFold.FFI.Rust.Poly ()
import ZkFold.FFI.Rust.RustBLS ()
import ZkFold.Protocol.Plonkup.Internal (
  Plonkup (..),
  PlonkupPermutationSize,
  PlonkupPolyExtendedLength,
 )
import ZkFold.Protocol.Plonkup.Prover.Polynomials (PlonkupCircuitPolynomials (..))
import ZkFold.Protocol.Plonkup.Relation (PlonkupRelation (..), toPlonkupRelation)
import ZkFold.Protocol.Plonkup.Verifier.Commitments (PlonkupCircuitCommitments (..))
import ZkFold.Symbolic.Class (Arithmetic)

data PlonkupSetup i o n g1 g2 pv = PlonkupSetup
  { omega :: !(ScalarFieldOf g1)
  , k1 :: !(ScalarFieldOf g1)
  , k2 :: !(ScalarFieldOf g1)
  , gs :: !(V.Vector g1)
  , h0 :: !g2
  , h1 :: !g2
  , sigma1s :: !(pv n)
  , sigma2s :: !(pv n)
  , sigma3s :: !(pv n)
  , relation :: !(PlonkupRelation i o n (ScalarFieldOf g1) pv)
  , polynomials :: !(PlonkupCircuitPolynomials n g1 pv)
  , commitments :: !(PlonkupCircuitCommitments g1)
  }

instance
  ( Show (PlonkupRelation i o n (ScalarFieldOf g1) pv)
  , Show (ScalarFieldOf g1)
  , Show (pv (PlonkupPolyExtendedLength n))
  , Show (pv n)
  , Show g1
  , Show g2
  )
  => Show (PlonkupSetup i o n g1 g2 pv)
  where
  show PlonkupSetup {..} =
    "Setup: "
      ++ show omega
      ++ " "
      ++ show k1
      ++ " "
      ++ show k2
      ++ " "
      ++ show gs
      ++ " "
      ++ show h0
      ++ " "
      ++ show h1
      ++ " "
      ++ show sigma1s
      ++ " "
      ++ show sigma2s
      ++ " "
      ++ show sigma3s
      ++ " "
      ++ show relation
      ++ " "
      ++ show polynomials
      ++ " "
      ++ show commitments

plonkupSetup
  :: forall i o n g1 g2 gt ts pv rustG1
   . ( Arithmetic (ScalarFieldOf g1)
     , Bilinear (V.Vector rustG1) (pv (PlonkupPolyExtendedLength n)) g1
     , Binary (Rep i)
     , Foldable o
     , KnownNat (PlonkupPolyExtendedLength n)
     , KnownNat n
     , Pairing g1 g2 gt
     , Representable i
     , Representable o
     , RustHaskell rustG1 g1
     , UnivariateFieldPolyVec (ScalarFieldOf g2) pv
     )
  => Plonkup i o n g1 g2 ts pv -> PlonkupSetup i o n g1 g2 pv
plonkupSetup Plonkup {..} =
  let !gs = toV gs'
      !gsR = h2r <$> gs
      !h0 = pointGen

      !relation@PlonkupRelation {..} = fromJust $ toPlonkupRelation ac :: PlonkupRelation i o n (ScalarFieldOf g1) pv

      f !i = case (i -! 1) `Prelude.div` value @n of
        0 -> omega ^ i
        1 -> k1 * (omega ^ i)
        2 -> k2 * (omega ^ i)
        _ -> error "setup: invalid index"

      !s = f <$> fromPermutation @(PlonkupPermutationSize n) sigma
      !sigma1s = toPolyVec $ V.take (fromIntegral $ value @n) s
      !sigma2s = toPolyVec $ V.take (fromIntegral $ value @n) $ V.drop (fromIntegral $ value @n) s
      !sigma3s = toPolyVec $ V.take (fromIntegral $ value @n) $ V.drop (fromIntegral $ 2 * value @n) s

      !qmX = polyVecInLagrangeBasis omega qM
      !qlX = polyVecInLagrangeBasis omega qL
      !qrX = polyVecInLagrangeBasis omega qR
      !qoX = polyVecInLagrangeBasis omega qO
      !qcX = polyVecInLagrangeBasis omega qC
      !qkX = polyVecInLagrangeBasis omega qK
      !t1X = polyVecInLagrangeBasis omega t1
      !t2X = polyVecInLagrangeBasis omega t2
      !t3X = polyVecInLagrangeBasis omega t3
      !s1X = polyVecInLagrangeBasis omega sigma1s
      !s2X = polyVecInLagrangeBasis omega sigma2s
      !s3X = polyVecInLagrangeBasis omega sigma3s
      !polynomials = PlonkupCircuitPolynomials {..}

      !com = bilinear
      !cmQl = gsR `com` qlX
      !cmQr = gsR `com` qrX
      !cmQo = gsR `com` qoX
      !cmQm = gsR `com` qmX
      !cmQc = gsR `com` qcX
      !cmQk = gsR `com` qkX
      !cmT1 = gsR `com` t1X
      !cmT2 = gsR `com` t2X
      !cmT3 = gsR `com` t3X
      !cmS1 = gsR `com` s1X
      !cmS2 = gsR `com` s2X
      !cmS3 = gsR `com` s3X
      !commitments = PlonkupCircuitCommitments {..}
   in PlonkupSetup {..}
