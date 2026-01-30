module ZkFold.Symbolic.Algorithm.Hash.MiMC (
  module ZkFold.Symbolic.Algorithm.Hash.MiMC,
  module ZkFold.Algorithm.Hash.MiMC,
) where

import Data.Foldable (toList)
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (Maybe (..))
import GHC.Generics (Par1 (..))
import Prelude (error, reverse)

import ZkFold.Algebra.Class
import ZkFold.Algorithm.Hash.MiMC
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.Package (unpacked)
import ZkFold.Symbolic.Class (Symbolic (..), fromCircuit2F)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Symbolic.MonadCircuit (newAssigned)

hash :: (SymbolicData x, Symbolic c) => x c -> FieldElement c
hash =
  mimcHashN mimcConstants zero
    . fmap FieldElement
    . unpacked
    . hmap toList
    . arithmetize

-- | Optimized MiMC-2n/n (Feistel) hash function for Symbolic context.
-- This implementation computes the round function with only 3 constraints
-- per round (for x², x⁴, and x⁵) instead of 6+ by inlining the additions.
mimcHash2S
  :: forall c. Symbolic c
  => [BaseField c] -> BaseField c -> FieldElement c -> FieldElement c -> FieldElement c
mimcHash2S xs k = case nonEmpty (reverse xs) of
  Just cs -> go cs
  Nothing -> error "mimcHash2S: empty list"
 where
  go :: NonEmpty (BaseField c) -> FieldElement c -> FieldElement c -> FieldElement c
  go (c :| cs) (FieldElement xL) (FieldElement xR) =
    let t5 = mimcRound k c xL xR
     in case nonEmpty cs of
          Just cs' -> go cs' t5 (FieldElement xL)
          Nothing -> t5

-- | Optimized MiMC round function: (xL + k + c)^5 + xR
-- This computes the entire round with only 3 polynomial constraints:
-- 1. t2 = (xL + k + c)^2
-- 2. t4 = t2^2
-- 3. result = t4 * (xL + k + c) + xR
mimcRound
  :: Symbolic c
  => BaseField c -> BaseField c -> c Par1 -> c Par1 -> FieldElement c
mimcRound k c xL xR = FieldElement $
  fromCircuit2F xL xR $ \(Par1 iL) (Par1 iR) -> do
    -- Compute t = xL + k + c (this is just witness computation, no constraint)
    -- Then compute t^2 = t * t (constraint 1)
    t2 <- newAssigned $ \w ->
      let t = w iL + fromConstant k + fromConstant c
       in t * t
    -- Compute t^4 = t2 * t2 (constraint 2)
    t4 <- newAssigned $ \w -> w t2 * w t2
    -- Compute result = t4 * t + xR = t^5 + xR (constraint 3)
    Par1 <$> newAssigned (\w ->
      let t = w iL + fromConstant k + fromConstant c
       in w t4 * t + w iR)

-- | Optimized MiMC hash for multiple inputs
mimcHashNS :: Symbolic c => [BaseField c] -> BaseField c -> [FieldElement c] -> FieldElement c
mimcHashNS xs k = go
 where
  go [] = mimcHash2S xs k zero zero
  go [z] = mimcHash2S xs k zero z
  go [zL, zR] = mimcHash2S xs k zL zR
  go (zL : zR : zs') = go (mimcHash2S xs k zL zR : zs')
