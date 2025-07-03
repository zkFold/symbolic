{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Payloaded where

import Data.Bifunctor (bimap)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Functor.Rep (Representable)
import Data.Tuple (snd)
import GHC.Generics (Par1 (..), U1 (..))

import ZkFold.Algebra.Class
import ZkFold.Control.HApplicative (hunit)
import ZkFold.Data.Eq
import ZkFold.Symbolic.Algorithm.Interpolation (interpolateW)
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (..), true)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))

newtype Payloaded f c = Payloaded {runPayloaded :: f (WitnessField c)}

instance Representable f => SymbolicData (Payloaded f) where
  type Layout (Payloaded f) _ = U1
  type Payload (Payloaded f) _ = f

  arithmetize _ = hunit
  payload = runPayloaded
  restore = Payloaded . snd
  interpolate bs (witnessF -> Par1 pt) =
    Payloaded $ interpolateW (bimap fromConstant runPayloaded <$> bs) pt

instance Representable f => SymbolicInput (Payloaded f) where
  isValid _ = true

instance Symbolic c => Eq (Payloaded f c) where
  type BooleanOf (Payloaded f c) = Bool c
  _ == _ = true
  _ /= _ = false
