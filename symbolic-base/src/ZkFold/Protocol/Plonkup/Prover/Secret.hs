{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Prover.Secret where

import           Data.Aeson.Types                            (FromJSON (..), ToJSON (..))
import           GHC.Generics                                (Generic)
import           Prelude                                     hiding (Num (..), drop, length, sum, take, (!!), (/), (^))
import           Test.QuickCheck                             (Arbitrary (..))

import           ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import           ZkFold.Algebra.EllipticCurve.Class     (CyclicGroup (..))
import           ZkFold.Data.Vector                     (Vector (..))

newtype PlonkupProverSecret g = PlonkupProverSecret (Vector 19 (ScalarFieldOf g))
    deriving stock Generic

deriving anyclass instance ToJSON   (PlonkupProverSecret BLS12_381_G1_Point)
deriving anyclass instance FromJSON (PlonkupProverSecret BLS12_381_G1_Point)

instance Show (ScalarFieldOf g) => Show (PlonkupProverSecret g) where
    show (PlonkupProverSecret v) =
        "PlonkupProverSecret: " ++ show v

instance Arbitrary (ScalarFieldOf g) => Arbitrary (PlonkupProverSecret g) where
    arbitrary = PlonkupProverSecret <$> arbitrary
