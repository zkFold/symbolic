{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Input where

import Data.List ((++))
import Text.Show (Show, show)

import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))

newtype PlonkupInput g = PlonkupInput {unPlonkupInput :: [ScalarFieldOf g]}

instance Show (ScalarFieldOf g) => Show (PlonkupInput g) where
  show (PlonkupInput v) = "Plonkup Input: " ++ show v
