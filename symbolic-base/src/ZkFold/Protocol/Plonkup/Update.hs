{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Update where

import           Data.Foldable                                (toList)
import           Data.Functor.Rep                             (Representable (..))
import           GHC.IsList                                   (fromList)
import           Prelude                                      hiding (Num (..), drop, length, pi, replicate, sum, take,
                                                               (!!), (/), (^))

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.Class           (ScalarFieldOf)
import           ZkFold.Algebra.Number
import           ZkFold.Algebra.Polynomial.Univariate         (UnivariateFieldPolyVec (..), UnivariateRingPolyVec (..),
                                                               toPolyVec)
import           ZkFold.Prelude                               (drop, length, take)
import qualified ZkFold.Protocol.Plonkup.Prover               as Prover
import           ZkFold.Protocol.Plonkup.Prover               (PlonkupProverSetup (..))
import           ZkFold.Protocol.Plonkup.Prover.Polynomials   (PlonkupCircuitPolynomials (..))
import           ZkFold.Protocol.Plonkup.Relation             (PlonkupRelation (..))
import qualified ZkFold.Protocol.Plonkup.Verifier             as Verifier
import           ZkFold.Protocol.Plonkup.Verifier             (PlonkupVerifierSetup (..))
import           ZkFold.Protocol.Plonkup.Verifier.Commitments (PlonkupCircuitCommitments (..))

updateRelation :: forall i o n a pv .
    ( Representable i
    , KnownNat n
    , UnivariateRingPolyVec a pv
    ) => PlonkupRelation i o n a pv -> [a] -> PlonkupRelation i o n a pv
updateRelation r@PlonkupRelation {..} inputs =
    let
        lmax = length $ pubInput $ tabulate $ const zero
        l = length inputs
        prvNum' = prvNum + l
        qC' = toPolyVec $ fromList $ concat
            [ take prvNum (toList $ fromPolyVec qC)
            , (negate <$> toList inputs)
            , drop l (toList $ fromPolyVec qC)
            ]
        pubInput' pi = drop l (pubInput pi)
    in
        if l > lmax
            then error "updateRelation: too many inputs"
            else r { qC = qC', pubInput = pubInput', prvNum = prvNum' }

updateProverSetup :: forall i o n g1 g2 pv .
    ( Representable i
    , KnownNat n
    , KnownNat ((4 * n) + 6)
    , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
    ) => PlonkupProverSetup i o n g1 g2 pv -> [ScalarFieldOf g1] -> PlonkupProverSetup i o n g1 g2 pv
updateProverSetup setup@PlonkupProverSetup {..} inputs =
    let
        relation'@PlonkupRelation {..} = updateRelation relation inputs
        polynomials' = polynomials { qcX = polyVecInLagrangeBasis omega qC }
    in
        setup { Prover.relation = relation', polynomials = polynomials' }

updateVerifierSetup :: forall i o n g1 g2 pv .
    ( Representable i
    , KnownNat n
    , AdditiveGroup g1
    , Scale (ScalarFieldOf g1) g1
    , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
    ) => PlonkupVerifierSetup i o n g1 g2 pv -> [ScalarFieldOf g1] -> [g1] -> PlonkupVerifierSetup i o n g1 g2 pv
updateVerifierSetup setup@PlonkupVerifierSetup {..} inputs hs =
    let
        relation' = updateRelation relation inputs
        PlonkupCircuitCommitments {..} = commitments
        commitments' = commitments { cmQc = cmQc - sum (zipWith scale inputs hs) }
    in
        setup { Verifier.relation = relation', commitments = commitments' }
