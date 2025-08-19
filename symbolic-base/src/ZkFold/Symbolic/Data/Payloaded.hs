{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Data.Payloaded where

import Data.Function (($), (.))
import Data.Functor ((<$>), fmap)
import Data.Tuple (snd)
import GHC.Generics (U1 (..), (:*:) (..))

import ZkFold.Control.HApplicative (hunit)
import ZkFold.Data.Eq
import ZkFold.Symbolic.Class (Symbolic (..), embedW)
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (..), true)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import ZkFold.Symbolic.WitnessContext (WitnessContext (..))

newtype Payloaded d c = Payloaded {runPayloaded :: d (WitnessContext c)}

payloaded :: (Symbolic c, SymbolicData d) => d c -> Payloaded d c
payloaded x = Payloaded $ restore (WC $ witnessF $ arithmetize x, payload x)

restored :: (Symbolic c, SymbolicData d) => Payloaded d c -> d c
restored (Payloaded y) = restore (embedW $ runWC $ arithmetize y, payload y)

instance SymbolicData d => SymbolicData (Payloaded d) where
  type Layout (Payloaded d) _ = U1
  type Payload (Payloaded d) n = Layout d n :*: Payload d n

  arithmetize _ = hunit
  payload (runPayloaded -> d) = runWC (arithmetize d) :*: payload d
  restore (snd -> l :*: p) = Payloaded $ restore (WC l, p)
  interpolate bs (WC . witnessF -> pt) =
    Payloaded $ interpolate (fmap runPayloaded <$> bs) pt

instance SymbolicData d => SymbolicInput (Payloaded d) where
  isValid _ = true

instance Symbolic c => Eq (Payloaded d c) where
  type BooleanOf (Payloaded d c) = Bool c
  _ == _ = true
  _ /= _ = false
