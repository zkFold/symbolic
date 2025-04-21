{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Verifier.Commitments where

import           Prelude hiding (Num (..), drop, length, sum, take, (!!), (/), (^))

data PlonkupCircuitCommitments g = PlonkupCircuitCommitments {
        cmQl :: g,
        cmQr :: g,
        cmQo :: g,
        cmQm :: g,
        cmQc :: g,
        cmQk :: g,
        cmT1 :: g,
        cmT2 :: g,
        cmT3 :: g,
        cmS1 :: g,
        cmS2 :: g,
        cmS3 :: g
    }
instance (Show g) => Show (PlonkupCircuitCommitments g) where
    show PlonkupCircuitCommitments {..} =
        "Plonkup Circuit Commitments: "
        ++ show cmQl ++ " "
        ++ show cmQr ++ " "
        ++ show cmQo ++ " "
        ++ show cmQm ++ " "
        ++ show cmQc ++ " "
        ++ show cmQk ++ " "
        ++ show cmT1 ++ " "
        ++ show cmT2 ++ " "
        ++ show cmT3 ++ " "
        ++ show cmS1 ++ " "
        ++ show cmS2 ++ " "
        ++ show cmS3

