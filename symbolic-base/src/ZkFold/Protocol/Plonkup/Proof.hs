{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Proof where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.Swagger (ToSchema (..))
import GHC.Generics (Generic)
import Prelude hiding (Num (..), drop, length, sum, take, (!!), (/), (^))

import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))

data PlonkupProof g = PlonkupProof
  { cmA :: !g
  , cmB :: !g
  , cmC :: !g
  , cmF :: !g
  , cmH1 :: !g
  , cmH2 :: !g
  , cmZ1 :: !g
  , cmZ2 :: !g
  , cmQlow :: !g
  , cmQmid :: !g
  , cmQhigh :: !g
  , proof1 :: !g
  , proof2 :: !g
  , a_xi :: !(ScalarFieldOf g)
  , b_xi :: !(ScalarFieldOf g)
  , c_xi :: !(ScalarFieldOf g)
  , s1_xi :: !(ScalarFieldOf g)
  , s2_xi :: !(ScalarFieldOf g)
  , f_xi :: !(ScalarFieldOf g)
  , t_xi :: !(ScalarFieldOf g)
  , t_xi' :: !(ScalarFieldOf g)
  , z1_xi' :: !(ScalarFieldOf g)
  , z2_xi' :: !(ScalarFieldOf g)
  , h1_xi' :: !(ScalarFieldOf g)
  , h2_xi :: !(ScalarFieldOf g)
  , l1_xi :: !(ScalarFieldOf g)
  , l_xi :: !([ScalarFieldOf g])
  -- ^ The denominator in the L_i polynomial evaluation
  }
  deriving Generic

deriving instance (Eq g, Eq (ScalarFieldOf g)) => Eq (PlonkupProof g)

instance (ToJSON g, ToJSON (ScalarFieldOf g)) => ToJSON (PlonkupProof g)

instance (FromJSON g, FromJSON (ScalarFieldOf g)) => FromJSON (PlonkupProof g)

instance (Typeable g, ToSchema g, s ~ ScalarFieldOf g, ToSchema s) => ToSchema (PlonkupProof g)

instance (Show (ScalarFieldOf g), Show g) => Show (PlonkupProof g) where
  show PlonkupProof {..} =
    "Plonkup Proof: "
      ++ show cmA
      ++ " "
      ++ show cmB
      ++ " "
      ++ show cmC
      ++ " "
      ++ show cmF
      ++ " "
      ++ show cmH1
      ++ " "
      ++ show cmH2
      ++ " "
      ++ show cmZ1
      ++ " "
      ++ show cmZ2
      ++ " "
      ++ show cmQlow
      ++ " "
      ++ show cmQmid
      ++ " "
      ++ show cmQhigh
      ++ " "
      ++ show proof1
      ++ " "
      ++ show proof2
      ++ " "
      ++ show a_xi
      ++ " "
      ++ show b_xi
      ++ " "
      ++ show c_xi
      ++ " "
      ++ show s1_xi
      ++ " "
      ++ show s2_xi
      ++ " "
      ++ show f_xi
      ++ " "
      ++ show t_xi
      ++ " "
      ++ show t_xi'
      ++ " "
      ++ show z1_xi'
      ++ " "
      ++ show z2_xi'
      ++ " "
      ++ show h1_xi'
      ++ " "
      ++ show h2_xi
      ++ " "
      ++ show l1_xi
      ++ " "
      ++ show l_xi
