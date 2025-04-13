{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Protocol.Plonkup.Proof where

import           Prelude                                 hiding (Num (..), drop, length, sum, take, (!!), (/), (^))

import           ZkFold.Base.Algebra.EllipticCurve.Class (CyclicGroup (..))

data PlonkupProof g = PlonkupProof {
        cmA     :: g,
        cmB     :: g,
        cmC     :: g,
        cmF     :: g,
        cmH1    :: g,
        cmH2    :: g,
        cmZ1    :: g,
        cmZ2    :: g,
        cmQlow  :: g,
        cmQmid  :: g,
        cmQhigh :: g,
        proof1  :: g,
        proof2  :: g,
        a_xi    :: ScalarFieldOf g,
        b_xi    :: ScalarFieldOf g,
        c_xi    :: ScalarFieldOf g,
        s1_xi   :: ScalarFieldOf g,
        s2_xi   :: ScalarFieldOf g,
        f_xi    :: ScalarFieldOf g,
        t_xi    :: ScalarFieldOf g,
        t_xi'   :: ScalarFieldOf g,
        z1_xi'  :: ScalarFieldOf g,
        z2_xi'  :: ScalarFieldOf g,
        h1_xi'  :: ScalarFieldOf g,
        h2_xi   :: ScalarFieldOf g,
        l1_xi   :: ScalarFieldOf g
        -- ^ The denominator in the L_1 polynomial evaluation
    }
instance (Show (ScalarFieldOf g), Show g) => Show (PlonkupProof g) where
    show PlonkupProof {..} =
        "Plonkup Proof: "
        ++ show cmA ++ " "
        ++ show cmB ++ " "
        ++ show cmC ++ " "
        ++ show cmF ++ " "
        ++ show cmH1 ++ " "
        ++ show cmH2 ++ " "
        ++ show cmZ1 ++ " "
        ++ show cmZ2 ++ " "
        ++ show cmQlow ++ " "
        ++ show cmQmid ++ " "
        ++ show cmQhigh ++ " "
        ++ show proof1 ++ " "
        ++ show proof2 ++ " "
        ++ show a_xi ++ " "
        ++ show b_xi ++ " "
        ++ show c_xi ++ " "
        ++ show s1_xi ++ " "
        ++ show s2_xi ++ " "
        ++ show f_xi ++ " "
        ++ show t_xi ++ " "
        ++ show t_xi' ++ " "
        ++ show z1_xi' ++ " "
        ++ show z2_xi' ++ " "
        ++ show h1_xi' ++ " "
        ++ show h2_xi ++ " "
        ++ show l1_xi
