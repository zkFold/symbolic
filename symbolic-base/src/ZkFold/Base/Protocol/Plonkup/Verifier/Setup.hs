{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Protocol.Plonkup.Verifier.Setup where

import           Prelude                                           hiding (Num (..), drop, length, sum, take, (!!), (/),
                                                                    (^))

import           ZkFold.Base.Algebra.EllipticCurve.Class           (CyclicGroup (..))
import           ZkFold.Base.Protocol.Plonkup.Relation             (PlonkupRelation (..))
import           ZkFold.Base.Protocol.Plonkup.Verifier.Commitments (PlonkupCircuitCommitments (..))

data PlonkupVerifierSetup i n w l g1 g2 pv = PlonkupVerifierSetup
    { omega       :: ScalarFieldOf g1
    , k1          :: ScalarFieldOf g1
    , k2          :: ScalarFieldOf g1
    , h1          :: g2
    , sigma1s     :: pv n
    , sigma2s     :: pv n
    , sigma3s     :: pv n
    , relation    :: PlonkupRelation i n w l (ScalarFieldOf g1) pv
    , commitments :: PlonkupCircuitCommitments g1
    }

instance
        ( CyclicGroup g1
        , Show g1
        , Show g2
        , Show (ScalarFieldOf g1)
        , Show (pv n)
        , Show (PlonkupRelation i n w l (ScalarFieldOf g1) pv)
        ) => Show (PlonkupVerifierSetup i n w l g1 g2 pv) where
    show PlonkupVerifierSetup {..} =
        "Verifier setup: "
        ++ show omega ++ " "
        ++ show k1 ++ " "
        ++ show k2 ++ " "
        ++ show h1 ++ " "
        ++ show sigma1s ++ " "
        ++ show sigma2s ++ " "
        ++ show sigma3s ++ " "
        ++ show relation ++ " "
        ++ show commitments
