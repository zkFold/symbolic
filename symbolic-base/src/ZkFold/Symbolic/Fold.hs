{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators         #-}

module ZkFold.Symbolic.Fold where

import           Control.DeepSeq       (NFData, NFData1)
import           Data.Binary           (Binary)
import           Data.Functor.Rep      (Representable (..))
import           Data.List.Infinite    (Infinite)
import           Data.Ord              (Ord)
import           Data.Traversable      (Traversable)
import           Data.Type.Equality    (type (~))
import           GHC.Generics          (Par1)

import           ZkFold.Symbolic.Class (Symbolic (..))

class Symbolic c => SymbolicFold c where
  -- | A function to perform folding in a generic context.
  --
  -- To do this, you need quite a few arguments, see documentation.
  -- Or, even better, use more high-level Symbolic 'List' API.
  sfoldl ::
    (Binary (Rep f), NFData (Rep f), Ord (Rep f)) =>
    (forall a. Binary a => Binary (f a)) =>
    (Representable f, NFData1 f, Traversable f) =>
    (Binary (Rep p), NFData (Rep p), Ord (Rep p), Representable p) =>
    (Binary (Rep g), NFData (Rep g), Ord (Rep g), Representable g) =>
    (forall a. Binary a => Binary (h a)) =>
    (WitnessField c ~ wc) =>
    (forall s.
      (SymbolicFold s, BaseField s ~ BaseField c) =>
      -- ^ In anonymous context over same base field,
      s f -> -- ^ given a current state layout,
      p (WitnessField s) -> -- ^ a current state payload
      s g -> -- ^ and next stream item,
      (s f, p (WitnessField s)) -- ^ compute the new state layout and payload.
    ) -- ^ A folding step. See arguments' documentation above.
    -> c f -- ^ Initial state layout.
    -> p wc -- ^ Initial state payload.
    -> c h -- ^ An identifier of a data stream (most likely a hash).
    -> Infinite (g wc) -- ^ data stream itself.
    -> c Par1 -- ^ A number of folding steps to perform.
    -> (c f, p wc) -- ^ Final state layout and payload after folding.
