{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Witness where

import Data.Functor ((<$>))
import Data.Functor.Classes (Show1)
import Data.List ((++))
import Test.QuickCheck (Arbitrary (..), Arbitrary1, arbitrary1)
import Text.Show (Show, show)

import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))

newtype PlonkupWitnessInput i g
  = PlonkupWitnessInput {witnessInput :: i (ScalarFieldOf g)}

instance
  (Show (ScalarFieldOf g), Show1 i)
  => Show (PlonkupWitnessInput i g)
  where
  show (PlonkupWitnessInput v) = "Plonkup Witness Input: " ++ show v

instance
  (Arbitrary (ScalarFieldOf g), Arbitrary1 i)
  => Arbitrary (PlonkupWitnessInput i g)
  where
  arbitrary = PlonkupWitnessInput <$> arbitrary1
