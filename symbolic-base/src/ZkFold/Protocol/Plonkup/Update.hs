{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Update where

import           Data.Foldable                                (toList)
import qualified Data.Vector                                  as V
import           GHC.IsList                                   (fromList)
import           Prelude                                      hiding (Num (..), drop, length, replicate, sum, take,
                                                               (!!), (/), (^))

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.Class           (ScalarFieldOf)
import           ZkFold.Algebra.Number
import           ZkFold.Algebra.Polynomial.Univariate         (UnivariateFieldPolyVec (..), UnivariateRingPolyVec (..),
                                                               toPolyVec)
import           ZkFold.Prelude                               (length, replicate, take)
import           ZkFold.Protocol.Plonkup.Internal             (PlonkupPolyExtended, PlonkupPolyExtendedLength)
import           ZkFold.Protocol.Plonkup.Prover.Polynomials   (PlonkupCircuitPolynomials (..))
import           ZkFold.Protocol.Plonkup.Relation             (PlonkupRelation (..))
-- import           ZkFold.Protocol.Plonkup.Setup                (PlonkupSetup (..))
import           ZkFold.Protocol.Plonkup.Verifier.Commitments (PlonkupCircuitCommitments (..))
import ZkFold.Protocol.Plonkup.Prover.Setup (PlonkupProverSetup (..))
import ZkFold.Protocol.Plonkup.Verifier (PlonkupVerifierSetup)
import ZkFold.Protocol.Plonkup.Verifier.Setup (PlonkupVerifierSetup(..))

nextGroupElement :: forall i o p n g1 g2 pv .
    ( KnownNat n
    , KnownNat ((4 * n) + 6)
    , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
    , Bilinear (V.Vector g1) (pv (PlonkupPolyExtendedLength n)) g1
    ) => PlonkupProverSetup i o p n g1 g2 pv -> g1
nextGroupElement PlonkupProverSetup {..} =
    let
        p :: PlonkupPolyExtended n g1 pv
        p = polyVecLagrange (value @n) (cNum relation + 1) omega
    in
        gs `bilinear` p

updateRelation :: forall i o p n a pv p' .
    ( KnownNat n
    , Foldable p'
    , UnivariateRingPolyVec a pv
    ) => PlonkupRelation i o p n a pv -> p' a -> PlonkupRelation i o p n a pv
updateRelation r@PlonkupRelation {..} inputs =
    let
        n = value @n
        l = length inputs
        qO' = toPolyVec $ fromList $ take cNum (toList $ fromPolyVec qO) ++ replicate l one ++ replicate (n -! (cNum + l)) zero
        qC' = toPolyVec $ fromList $ take cNum (toList $ fromPolyVec qC) ++ (negate <$> toList inputs) ++ replicate (n -! (cNum + l)) zero
    in
        r { qO = qO', qC = qC', cNum = cNum + l }

updateProverSetup :: forall i o p n g1 g2 pv p' .
    ( KnownNat n
    , KnownNat ((4 * n) + 6)
    , Foldable p'
    , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
    ) => PlonkupProverSetup i o p n g1 g2 pv -> p' (ScalarFieldOf g1) -> PlonkupProverSetup i o p n g1 g2 pv
updateProverSetup setup@PlonkupProverSetup {..} inputs =
    let
        relation'@PlonkupRelation {..} = updateRelation relation inputs

        qoX' = polyVecInLagrangeBasis omega qO
        qcX' = polyVecInLagrangeBasis omega qC

        polynomials' = polynomials { qoX = qoX', qcX = qcX' }

        -- commitments' = commitments { cmQl = gs `bilinear` qlX', cmQc = gs `bilinear` qcX' }
    in
        setup { relation = relation' , polynomials = polynomials' }

updateVerifierSetup :: forall i o p n g1 g2 pv p' .
    ( KnownNat n
    , Foldable p'
    , AdditiveGroup g1
    , Scale (ScalarFieldOf g1) g1
    , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
    ) => PlonkupVerifierSetup i o p n g1 g2 pv -> p' (ScalarFieldOf g1) -> p' g1 -> PlonkupVerifierSetup i o p n g1 g2 pv
updateVerifierSetup setup@PlonkupVerifierSetup {..} inputs hs =
    let
        relation' = updateRelation relation inputs
        PlonkupCircuitCommitments {..} = commitments

        commitments' = commitments { cmQo = cmQo + sum hs, cmQc = cmQc - sum (zipWith scale (toList inputs) (toList hs)) }
    in
        setup { relation = relation' , commitments = commitments' }