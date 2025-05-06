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

newtype PlonkupInput o g = PlonkupInput { unPlonkupInput :: o (ScalarFieldOf g) }

instance (Show1 o, Show (ScalarFieldOf g)) => Show (PlonkupInput o g) where
    show (PlonkupInput v) = "Plonkup Input: " ++ show v

instance (Arbitrary (o (ScalarFieldOf g))) => Arbitrary (PlonkupInput o g) where
    arbitrary = PlonkupInput <$> arbitrary

plonkupVerifierInput ::
  (Functor o, Field (ScalarFieldOf g)) => o (ScalarFieldOf g) -> PlonkupInput o g
plonkupVerifierInput input = PlonkupInput $ negate <$> input
