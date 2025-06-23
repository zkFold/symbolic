{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Verifier.Setup where

import Prelude hiding (Num (..), drop, length, sum, take, (!!), (/), (^))

import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import ZkFold.Protocol.Plonkup.Relation (PlonkupRelation (..))
import ZkFold.Protocol.Plonkup.Verifier.Commitments (PlonkupCircuitCommitments (..))

data PlonkupVerifierSetup i o n g1 g2 pv = PlonkupVerifierSetup
  { omega :: !(ScalarFieldOf g1)
  , k1 :: !(ScalarFieldOf g1)
  , k2 :: !(ScalarFieldOf g1)
  , h1 :: !g2
  , sigma1s :: !(pv n)
  , sigma2s :: !(pv n)
  , sigma3s :: !(pv n)
  , relation :: !(PlonkupRelation i o n (ScalarFieldOf g1) pv)
  , commitments :: !(PlonkupCircuitCommitments g1)
  }

instance
  ( CyclicGroup g1
  , Show (PlonkupRelation i o n (ScalarFieldOf g1) pv)
  , Show (ScalarFieldOf g1)
  , Show (pv n)
  , Show g1
  , Show g2
  )
  => Show (PlonkupVerifierSetup i o n g1 g2 pv)
  where
  show PlonkupVerifierSetup {..} =
    "Verifier setup: "
      ++ show omega
      ++ " "
      ++ show k1
      ++ " "
      ++ show k2
      ++ " "
      ++ show h1
      ++ " "
      ++ show sigma1s
      ++ " "
      ++ show sigma2s
      ++ " "
      ++ show sigma3s
      ++ " "
      ++ show relation
      ++ " "
      ++ show commitments
