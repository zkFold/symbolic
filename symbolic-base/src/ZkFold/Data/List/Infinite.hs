{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Data.List.Infinite where

import Data.Distributive (Distributive (..))
import Data.Function ((.))
import Data.Functor.Rep (Representable (..), distributeRep)
import Data.List.Infinite (Infinite)
import qualified Data.List.Infinite as Inf
import GHC.Real (fromIntegral)
import Numeric.Natural (Natural)

instance Distributive Infinite where
  distribute = distributeRep

instance Representable Infinite where
  type Rep Infinite = Natural
  tabulate = Inf.tabulate . (. fromIntegral)
  index = (. fromIntegral) . (Inf.!!)
