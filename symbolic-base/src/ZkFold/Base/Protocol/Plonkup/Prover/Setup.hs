{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Protocol.Plonkup.Prover.Setup where

import qualified Data.Vector                                     as V
import           Prelude                                         hiding (Num (..), drop, length, sum, take, (!!), (/),
                                                                  (^))

import           ZkFold.Base.Algebra.EllipticCurve.Class         (CyclicGroup (..))
import           ZkFold.Base.Protocol.Plonkup.Internal           (PlonkupPolyExtendedLength)
import           ZkFold.Base.Protocol.Plonkup.Prover.Polynomials
import           ZkFold.Base.Protocol.Plonkup.Relation           (PlonkupRelation (..))

data PlonkupProverSetup i n l g1 g2 pv = PlonkupProverSetup
    { omega       :: ScalarFieldOf g1
    , k1          :: ScalarFieldOf g1
    , k2          :: ScalarFieldOf g1
    , gs          :: V.Vector g1
    , sigma1s     :: pv n
    , sigma2s     :: pv n
    , sigma3s     :: pv n
    , relation    :: PlonkupRelation i n l (ScalarFieldOf g1) pv
    , polynomials :: PlonkupCircuitPolynomials n g1 pv
    }

instance
        ( CyclicGroup g1
        , Show g1
        , Show g2
        , Show (ScalarFieldOf g1)
        , Show (pv n)
        , Show (pv (PlonkupPolyExtendedLength n))
        , Show (PlonkupRelation i n l (ScalarFieldOf g1) pv)
        ) => Show (PlonkupProverSetup i n l g1 g2 pv) where
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
