{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Update where

import           Data.Foldable                                       (toList)
import qualified Data.Vector                                         as V
import           GHC.IsList                                          (fromList)
import           Prelude                                             hiding (Num (..), drop, length, replicate, sum,
                                                                      take, (!!), (/), (^))

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Number
import           ZkFold.Algebra.Polynomial.Univariate                (UnivariateRingPolyVec (..), toPolyVec, UnivariateFieldPolyVec (..))
import           ZkFold.Prelude                                      (length, replicate, take)
import           ZkFold.Protocol.Plonkup.Internal                    (PlonkupPolyExtendedLength, PlonkupPolyExtended)
import ZkFold.Protocol.Plonkup.Relation (PlonkupRelation (..))
import ZkFold.Protocol.Plonkup.Setup (PlonkupSetup (..))
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Protocol.Plonkup.Prover.Polynomials (PlonkupCircuitPolynomials (..))
import ZkFold.Protocol.Plonkup.Verifier.Commitments (PlonkupCircuitCommitments (..))

nextGroupElement :: forall i o p n g1 g2 pv .
    ( KnownNat n
    , KnownNat ((4 * n) + 6)
    , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
    , Bilinear (V.Vector g1) (pv (PlonkupPolyExtendedLength n)) g1
    ) => PlonkupSetup i o p n g1 g2 pv -> g1
nextGroupElement PlonkupSetup {..} =
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
        qL' = toPolyVec $ fromList $ take cNum (toList $ fromPolyVec qL) ++ replicate l one ++ replicate (n -! (cNum + l)) zero
        qC' = toPolyVec $ fromList $ take cNum (toList $ fromPolyVec qC) ++ (toList inputs) ++ replicate (n -! (cNum + l)) zero
    in
        r { qL = qL', qC = qC', cNum = cNum + l }

updateSetup :: forall i o p n g1 g2 pv p' .
    ( KnownNat n
    , KnownNat ((4 * n) + 6)
    , Foldable p'
    , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
    , Bilinear (V.Vector g1) (pv (PlonkupPolyExtendedLength n)) g1
    ) => PlonkupSetup i o p n g1 g2 pv -> p' (ScalarFieldOf g1) -> PlonkupSetup i o p n g1 g2 pv
updateSetup setup@PlonkupSetup {..} inputs =
    let
        relation'@PlonkupRelation {..} = updateRelation relation inputs

        qlX' = polyVecInLagrangeBasis omega qL
        qcX' = polyVecInLagrangeBasis omega qC

        polynomials' = polynomials { qlX = qlX', qcX = qcX' }

        commitments' = commitments { cmQl = gs `bilinear` qlX', cmQc = gs `bilinear` qcX' }        
    in
        setup { relation = relation' , polynomials = polynomials', commitments = commitments' }
