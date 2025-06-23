{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Fold where

import Control.DeepSeq (NFData1)
import Data.Binary (Binary)
import Data.Functor.Rep (Representable (..))
import Data.List.Infinite (Infinite)
import Data.Traversable (Traversable)
import Data.Type.Equality (type (~))
import GHC.Generics (Par1)
import ZkFold.Data.ByteString (Binary1)
import ZkFold.Symbolic.Class (Symbolic (..))

class Symbolic c => SymbolicFold c where
  -- | A function to perform folding in a generic context.
  --
  --   To do this, you need quite a few arguments, see documentation.
  --   Or, even better, use more high-level Symbolic 'List' API.
  sfoldl
    :: (Binary (Rep f), Binary1 f, NFData1 f, Representable f, Traversable f)
    => (Binary (Rep g), Binary (Rep p), Representable g, Representable p)
    => (Binary1 h, WitnessField c ~ wc)
    => ( forall s
          . (BaseField s ~ BaseField c, SymbolicFold s)
         => -- \^ In anonymous context over same base field,
         s f
         -- \^ given a current state layout,
         -> p (WitnessField s)
         -- \^ a current state payload
         -> s g
         -- \^ and next stream item,
         -> (s f, p (WitnessField s))
       )
    -- ^ A folding step. See arguments' documentation above.

    -- \^ compute the new state layout and payload.

    -> c f
    -- ^ Initial state layout.
    -> p wc
    -- ^ Initial state payload.
    -> c h
    -- ^ An identifier of a data stream (most likely a hash).
    -> Infinite (g wc)
    -- ^ data stream itself.
    -> c Par1
    -- ^ A number of folding steps to perform.
    -> (c f, p wc)
    -- ^ Final state layout and payload after folding.
