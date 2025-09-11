{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Payloaded where

import Data.Bifunctor (bimap)
import Data.Function (($), (.))
import Data.Functor (Functor, fmap, (<&>))
import Data.Semialign (Semialign)
import Data.Tuple (snd)
import GHC.Generics (Par1 (..), U1 (..), (:*:) (..), (:.:) (..))

import ZkFold.Algebra.Class (Order, fromConstant)
import ZkFold.Control.HApplicative (hunit)
import ZkFold.Data.Eq
import ZkFold.Data.Product (fromPair, toPair)
import ZkFold.Symbolic.Algorithm.Interpolation (interpolateW)
import ZkFold.Symbolic.Class (Symbolic (..), embedW)
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (..), true)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import Data.Functor.Classes (Show1)
import Text.Show (Show)

newtype Payloaded f d c = Payloaded
  { runPayloaded
      :: f
           ( Layout d (Order (BaseField c)) (WitnessField c)
           , Payload d (Order (BaseField c)) (WitnessField c)
           )
  }

deriving instance
  ( Show1 f, Show (Layout d (Order (BaseField c)) (WitnessField c))
  , Show (Payload d (Order (BaseField c)) (WitnessField c))
  ) => Show (Payloaded f d c)

payloaded
  :: forall f d c
   . (Functor f, Symbolic c, SymbolicData d)
  => f (d c) -> Payloaded f d c
payloaded xs = Payloaded $ xs <&> \x -> (witnessF $ arithmetize x, payload x)

restored
  :: forall f d c
   . (Functor f, Symbolic c, SymbolicData d)
  => Payloaded f d c -> f (d c)
restored xs = runPayloaded xs <&> \(l, p) -> restore (embedW l, p)

instance (Semialign f, SymbolicData d) => SymbolicData (Payloaded f d) where
  type Layout (Payloaded f d) _ = U1
  type Payload (Payloaded f d) n = f :.: (Layout d n :*: Payload d n)
  type HasRep (Payloaded f d) c = (RepPayload f, HasRep d c)

  arithmetize _ = hunit
  payload = Comp1 . fmap fromPair . runPayloaded
  restore = Payloaded . fmap toPair . unComp1 . snd
  interpolate (fmap (bimap fromConstant payload) -> bs) (pt :: c p) =
    restore (hunit, interpolateW bs $ unPar1 $ witnessF pt)

instance (Semialign f, SymbolicData d) => SymbolicInput (Payloaded f d) where
  isValid _ = true

instance Symbolic c => Eq (Payloaded f d c) where
  type BooleanOf (Payloaded f d c) = Bool c
  _ == _ = true
  _ /= _ = false
