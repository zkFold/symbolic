{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Protocol.Plonkup.Verifier.Setup where


import           Control.DeepSeq                                   (NFData)
import           GHC.Generics                                      (Generic)
import           Prelude                                           hiding (Num (..), drop, length, sum, take, (!!), (/),
                                                                    (^))

import           ZkFold.Base.Algebra.EllipticCurve.Class           (CyclicGroup (..))
import           ZkFold.Base.Algebra.Polynomials.Univariate        hiding (qr)
import           ZkFold.Base.Protocol.Plonkup.Relation             (PlonkupRelation (..))
import           ZkFold.Base.Protocol.Plonkup.Verifier.Commitments (PlonkupCircuitCommitments (..))

data PlonkupVerifierSetup p i n l g1 g2 = PlonkupVerifierSetup
    { omega       :: ScalarFieldOf g1
    , k1          :: ScalarFieldOf g1
    , k2          :: ScalarFieldOf g1
    , h1          :: g2
    , sigma1s     :: PolyVec (ScalarFieldOf g1) n
    , sigma2s     :: PolyVec (ScalarFieldOf g1) n
    , sigma3s     :: PolyVec (ScalarFieldOf g1) n
    , relation    :: PlonkupRelation p i n l (ScalarFieldOf g1)
    , commitments :: PlonkupCircuitCommitments g1
    }
    deriving Generic

deriving instance (NFData g1, NFData g2, NFData (ScalarFieldOf g1)) => NFData (PlonkupVerifierSetup p i n l g1 g2)

instance
        ( CyclicGroup g1
        , Show g1
        , Show g2
        , Show (ScalarFieldOf g1)
        , Show (PlonkupRelation p i n l (ScalarFieldOf g1))
        ) => Show (PlonkupVerifierSetup p i n l g1 g2) where
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
