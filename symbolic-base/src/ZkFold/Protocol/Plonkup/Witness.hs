{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Witness where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.Functor ((<$>))
import Data.Functor.Classes (Show1)
import Data.List ((++))
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..), Arbitrary1, arbitrary1)
import Text.Show (Show, show)

import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))

newtype PlonkupWitnessInput i g
  = PlonkupWitnessInput {witnessInput :: i (ScalarFieldOf g)}
  deriving Generic

instance (s ~ ScalarFieldOf g, ToJSON (i s)) => ToJSON (PlonkupWitnessInput i g)

instance (s ~ ScalarFieldOf g, FromJSON (i s)) => FromJSON (PlonkupWitnessInput i g)

instance (Typeable i, Typeable g, s ~ ScalarFieldOf g, ToSchema (i s)) => ToSchema (PlonkupWitnessInput i g)

instance
  (Show1 i, Show (ScalarFieldOf g))
  => Show (PlonkupWitnessInput i g)
  where
  show (PlonkupWitnessInput v) = "Plonkup Witness Input: " ++ show v

instance
  (Arbitrary1 i, Arbitrary (ScalarFieldOf g))
  => Arbitrary (PlonkupWitnessInput i g)
  where
  arbitrary = PlonkupWitnessInput <$> arbitrary1
