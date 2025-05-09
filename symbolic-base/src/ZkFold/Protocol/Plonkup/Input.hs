{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Input where

import           Data.Function                      (($))
import           Data.Functor                       ((<$>))
import           Data.List                          ((++))
import           Test.QuickCheck                    (Arbitrary (..))
import           Text.Show                          (Show, show)

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))

newtype PlonkupInput g = PlonkupInput { unPlonkupInput :: [ScalarFieldOf g] }

instance Show (ScalarFieldOf g) => Show (PlonkupInput g) where
    show (PlonkupInput v) = "Plonkup Input: " ++ show v

instance Arbitrary (ScalarFieldOf g) => Arbitrary (PlonkupInput g) where
    arbitrary = PlonkupInput <$> arbitrary

plonkupVerifierInput :: Field (ScalarFieldOf g) => [ScalarFieldOf g] -> PlonkupInput g
plonkupVerifierInput input = PlonkupInput $ negate <$> input
