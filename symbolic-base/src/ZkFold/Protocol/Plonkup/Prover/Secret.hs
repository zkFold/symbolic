{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Prover.Secret where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import Data.Data (Typeable)
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Prelude hiding (Num (..), drop, length, sum, take, (!!), (/), (^))

import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import ZkFold.Data.Vector (Vector (..))

newtype PlonkupProverSecret g = PlonkupProverSecret (Vector 19 (ScalarFieldOf g))
  deriving stock Generic

instance ToJSON (ScalarFieldOf g) => ToJSON (PlonkupProverSecret g)

instance FromJSON (ScalarFieldOf g) => FromJSON (PlonkupProverSecret g)

deriving newtype instance (Typeable g, s ~ ScalarFieldOf g, ToSchema s) => ToSchema (PlonkupProverSecret g)

instance Show (ScalarFieldOf g) => Show (PlonkupProverSecret g) where
  show (PlonkupProverSecret v) =
    "PlonkupProverSecret: " ++ show v

instance Arbitrary (ScalarFieldOf g) => Arbitrary (PlonkupProverSecret g) where
  arbitrary = PlonkupProverSecret <$> arbitrary
