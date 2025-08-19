{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.WitnessContext where

import ZkFold.Symbolic.Class
import ZkFold.Data.HFunctor
import ZkFold.Control.HApplicative
import ZkFold.Symbolic.Interpreter
import Control.DeepSeq (NFData (..), NFData1, liftRnf)
import ZkFold.Data.HFunctor.Classes (HNFData, hliftRnf)
import ZkFold.Data.Package
import Data.Functor ((<$>))
import Data.Function (($))
import Data.Functor.Identity (Identity (..))

newtype WitnessContext c f = WC { runWC :: f (WitnessField c) }
  deriving (HFunctor, HApplicative) via (Interpreter (WitnessField c))

deriving via (Interpreter (WitnessField c)) instance
  NFData (WitnessField c) => HNFData (WitnessContext c)

instance (NFData1 f, NFData (WitnessField c)) => NFData (WitnessContext c f) where
  rnf = hliftRnf liftRnf

instance Package (WitnessContext c) where
  unpackWith f (WC g) = WC <$> f g
  packWith f g = WC $ f $ runWC <$> g

instance Symbolic c => Symbolic (WitnessContext c) where
  type BaseField (WitnessContext c) = BaseField c
  type WitnessField (WitnessContext c) = WitnessField c
  witnessF = runWC
  fromCircuitF (WC f) g = WC $ runIdentity <$>
    runWitnesses @(BaseField c) @(WitnessField c) (g $ Identity <$> f)
