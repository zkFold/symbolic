{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Input where

import           Data.Function                      (($))
import           Data.Functor                       (Functor, (<$>))
import           Data.Functor.Classes               (Show1)
import           Data.List                          ((++))
import           Test.QuickCheck                    (Arbitrary (..))
import           Text.Show                          (Show, show)

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import           ZkFold.Symbolic.Compiler           ()

newtype PlonkupInput l g = PlonkupInput { unPlonkupInput :: l (ScalarFieldOf g) }

instance (Show1 l, Show (ScalarFieldOf g)) => Show (PlonkupInput l g) where
    show (PlonkupInput v) = "Plonkup Input: " ++ show v

instance (Arbitrary (l (ScalarFieldOf g))) => Arbitrary (PlonkupInput l g) where
    arbitrary = PlonkupInput <$> arbitrary

plonkupVerifierInput ::
  (Functor l, Field (ScalarFieldOf g)) => l (ScalarFieldOf g) -> PlonkupInput l g
plonkupVerifierInput input = PlonkupInput $ negate <$> input
