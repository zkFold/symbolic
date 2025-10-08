{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.WitnessContext where

import Control.DeepSeq (NFData (..), NFData1, liftRnf)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Functor.Identity (Identity (..))

import ZkFold.Control.HApplicative
import ZkFold.Data.HFunctor
import ZkFold.Data.HFunctor.Classes (HNFData, hliftRnf)
import ZkFold.Data.Package
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Interpreter

newtype WitnessContext c f = WC {runWC :: f (WitnessField c)}
  deriving (HApplicative, HFunctor) via (Interpreter (WitnessField c))

deriving via
  (Interpreter (WitnessField c))
  instance
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
  fromCircuitF (WC f) g =
    WC $
      runIdentity
        <$> runWitnesses @(BaseField c) @(WitnessField c) (g $ Identity <$> f)

toWitnessContext :: forall c x. Symbolic c => SymbolicData x => x c -> x (WitnessContext c)
toWitnessContext x =
  restore (embedW $ witnessF $ arithmetize x, payload x)
