{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Update where

import           Data.Foldable                                (toList)
import qualified Data.Vector                                  as V
import           GHC.IsList                                   (fromList)
import           Prelude                                      hiding (Num (..), drop, length, replicate, sum, take, pi,
                                                               (!!), (/), (^))

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.Class           (ScalarFieldOf)
import           ZkFold.Algebra.Number
import           ZkFold.Algebra.Polynomial.Univariate         (UnivariateFieldPolyVec (..), UnivariateRingPolyVec (..),
                                                               toPolyVec)
import           ZkFold.Prelude                               (length, take, drop)
import           ZkFold.Protocol.Plonkup.Internal             (PlonkupPolyExtended, PlonkupPolyExtendedLength)
import           ZkFold.Protocol.Plonkup.Prover.Polynomials   (PlonkupCircuitPolynomials (..))
import           ZkFold.Protocol.Plonkup.Prover.Setup         (PlonkupProverSetup (..))
import           ZkFold.Protocol.Plonkup.Relation             (PlonkupRelation (..))
import           ZkFold.Protocol.Plonkup.Verifier             (PlonkupVerifierSetup)
import           ZkFold.Protocol.Plonkup.Verifier.Commitments (PlonkupCircuitCommitments (..))
import           ZkFold.Protocol.Plonkup.Verifier.Setup       (PlonkupVerifierSetup (..))

nextGroupElement :: forall i o n g1 g2 pv .
    ( KnownNat n
    , KnownNat ((4 * n) + 6)
    , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
    , Bilinear (V.Vector g1) (pv (PlonkupPolyExtendedLength n)) g1
    ) => PlonkupProverSetup i o n g1 g2 pv -> g1
nextGroupElement PlonkupProverSetup {..} =
    let
        p :: PlonkupPolyExtended n g1 pv
        p = polyVecLagrange (value @n) (prvNum relation + 1) omega
    in
        gs `bilinear` p

updateRelation :: forall i o n a pv p' .
    ( KnownNat n
    , Foldable p'
    , UnivariateRingPolyVec a pv
    ) => PlonkupRelation i o n a pv -> p' a -> PlonkupRelation i o n a pv
updateRelation r@PlonkupRelation {..} inputs =
    let
        l = length inputs
        prvNum' = prvNum + l
        qC' = toPolyVec $ fromList $ concat
            [ take prvNum (toList $ fromPolyVec qC)
            , (negate <$> toList inputs)
            , drop l (toList $ fromPolyVec qC)
            ]
        pubInput' pi = drop l (pubInput pi)
    in
        r { qC = qC', pubInput = pubInput', prvNum = prvNum' }

updateProverSetup :: forall i o n g1 g2 pv p' .
    ( KnownNat n
    , KnownNat ((4 * n) + 6)
    , Foldable p'
    , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
    ) => PlonkupProverSetup i o n g1 g2 pv -> p' (ScalarFieldOf g1) -> PlonkupProverSetup i o n g1 g2 pv
updateProverSetup setup@PlonkupProverSetup {..} inputs =
    let
        relation'@PlonkupRelation {..} = updateRelation relation inputs
        polynomials' = polynomials { qcX = polyVecInLagrangeBasis omega qC }
    in
        setup { relation = relation', polynomials = polynomials' }

updateVerifierSetup :: forall i o n g1 g2 pv p' .
    ( KnownNat n
    , Foldable p'
    , AdditiveGroup g1
    , Scale (ScalarFieldOf g1) g1
    , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
    ) => PlonkupVerifierSetup i o n g1 g2 pv -> p' (ScalarFieldOf g1) -> p' g1 -> PlonkupVerifierSetup i o n g1 g2 pv
updateVerifierSetup setup@PlonkupVerifierSetup {..} inputs hs =
    let
        relation' = updateRelation relation inputs
        PlonkupCircuitCommitments {..} = commitments
        commitments' = commitments { cmQc = cmQc - sum (zipWith scale (toList inputs) (toList hs)) }
    in
        setup { relation = relation', commitments = commitments' }
