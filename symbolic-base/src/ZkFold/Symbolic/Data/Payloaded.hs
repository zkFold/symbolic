{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Payloaded where

import Data.Bifunctor (Bifunctor (first))
import Data.Functor (fmap, (<$>))
import Data.Semialign (Semialign)
import Data.Tuple (snd)
import GHC.Generics (Par1 (Par1), U1 (..), (:*:) (..))

import ZkFold.Algebra.Class (Order, fromConstant)
import ZkFold.Control.HApplicative (hunit)
import ZkFold.Data.Eq
import ZkFold.Symbolic.Algorithm.Interpolation (interpolateW)
import ZkFold.Symbolic.Class (Symbolic (..), embedW)
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (..), true)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))

data Payloaded d c = Payloaded
  { pdLayout :: Layout d (Order (BaseField c)) (WitnessField c)
  , pdPayload :: Payload d (Order (BaseField c)) (WitnessField c)
  }

payloaded :: (Symbolic c, SymbolicData d) => d c -> Payloaded d c
payloaded x = witnessF (arithmetize x) `Payloaded` payload x

restored :: (Symbolic c, SymbolicData d) => Payloaded d c -> d c
restored (Payloaded l p) = restore (embedW l, p)

class (Semialign (Layout d n), Semialign (Payload d n)) => Payloadable d n

instance (Semialign (Layout d n), Semialign (Payload d n)) => Payloadable d n

instance (forall n. Payloadable d n) => SymbolicData (Payloaded d) where
  type Layout (Payloaded d) _ = U1
  type Payload (Payloaded d) n = Layout d n :*: Payload d n

  arithmetize _ = hunit
  payload d = pdLayout d :*: pdPayload d
  restore (snd -> l :*: p) = Payloaded l p
  interpolate (fmap (first fromConstant) -> bs) (witnessF -> Par1 pt) =
    interpolateW (fmap pdLayout <$> bs) pt
      `Payloaded` interpolateW (fmap pdPayload <$> bs) pt

instance (forall n. Payloadable d n) => SymbolicInput (Payloaded d) where
  isValid _ = true

instance Symbolic c => Eq (Payloaded d c) where
  type BooleanOf (Payloaded d c) = Bool c
  _ == _ = true
  _ /= _ = false
