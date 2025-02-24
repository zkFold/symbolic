{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Protocol.Plonkup.Prover.Setup where

import           Control.DeepSeq                                 (NFData)
import qualified Data.Vector                                     as V
import           GHC.Generics                                    (Generic)
import           Prelude                                         hiding (Num (..), drop, length, sum, take, (!!), (/),
                                                                  (^))

import           ZkFold.Base.Algebra.EllipticCurve.Class         (CyclicGroup (..))
import           ZkFold.Base.Algebra.Polynomials.Univariate      hiding (qr)
import           ZkFold.Base.Protocol.Plonkup.Prover.Polynomials
import           ZkFold.Base.Protocol.Plonkup.Relation           (PlonkupRelation (..))

data PlonkupProverSetup p i n l g1 g2 = PlonkupProverSetup
    { omega       :: ScalarFieldOf g1
    , k1          :: ScalarFieldOf g1
    , k2          :: ScalarFieldOf g1
    , gs          :: V.Vector g1
    , sigma1s     :: PolyVec (ScalarFieldOf g1) n
    , sigma2s     :: PolyVec (ScalarFieldOf g1) n
    , sigma3s     :: PolyVec (ScalarFieldOf g1) n
    , relation    :: PlonkupRelation p i n l (ScalarFieldOf g1)
    , polynomials :: PlonkupCircuitPolynomials n g1
    }
    deriving Generic

deriving instance (NFData g1, NFData (ScalarFieldOf g1)) => NFData (PlonkupProverSetup p i n l g1 g2)

instance
        ( CyclicGroup g1
        , Show g1
        , Show g2
        , Show (ScalarFieldOf g1)
        , Show (PlonkupRelation p i n l (ScalarFieldOf g1))
        ) => Show (PlonkupProverSetup p i n l g1 g2) where
    show PlonkupProverSetup {..} =
        "Prover setup: "
        ++ show omega ++ " "
        ++ show k1 ++ " "
        ++ show k2 ++ " "
        ++ show gs ++ " "
        ++ show sigma1s ++ " "
        ++ show sigma2s ++ " "
        ++ show sigma3s ++ " "
        ++ show relation ++ " "
        ++ show polynomials
