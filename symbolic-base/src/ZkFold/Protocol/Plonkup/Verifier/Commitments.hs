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
        cmS1 :: g,
        cmS2 :: g,
        cmS3 :: g,
        cmT1 :: g
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
        ++ show cmS1 ++ " "
        ++ show cmS2 ++ " "
        ++ show cmS3 ++ " "
        ++ show cmT1
