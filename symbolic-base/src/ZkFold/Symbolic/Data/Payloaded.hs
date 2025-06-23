{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Payloaded where

import Data.Bifunctor (bimap)
import Data.Function (const, ($), (.))
import Data.Functor ((<$>))
import Data.Proxy (Proxy (..))
import Data.Tuple (snd)
import GHC.Generics (Par1 (..), U1 (..))

import ZkFold.Algebra.Class
import ZkFold.Control.HApplicative (hunit)
import ZkFold.Symbolic.Algorithm.Interpolation (interpolateW)
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (..), true)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Eq
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))

newtype Payloaded f c = Payloaded {runPayloaded :: f (WitnessField c)}

instance (Symbolic c, PayloadFunctor f) => SymbolicData (Payloaded f c) where
  type Context (Payloaded f c) = c
  type Support (Payloaded f c) = Proxy c
  type Layout (Payloaded f c) = U1
  type Payload (Payloaded f c) = f

  arithmetize _ _ = hunit
  payload = const . runPayloaded
  interpolate bs (witnessF -> Par1 pt) =
    Payloaded $ interpolateW (bimap fromConstant runPayloaded <$> bs) pt
  restore = Payloaded . snd . ($ Proxy)

instance (Symbolic c, PayloadFunctor f) => SymbolicInput (Payloaded f c) where
  isValid = const true

instance Symbolic c => Eq (Payloaded f c) where
  type BooleanOf (Payloaded f c) = Bool c
  _ == _ = true
  _ /= _ = false
