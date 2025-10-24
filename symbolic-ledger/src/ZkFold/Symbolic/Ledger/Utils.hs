module ZkFold.Symbolic.Ledger.Utils (
  replaceFirstMatchWith,
  replaceFirstMatchWith',
  unsafeToVector',
) where

import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat)
import ZkFold.Control.Conditional
import ZkFold.Data.Bool
import ZkFold.Data.Eq
import ZkFold.Data.Vector
import Prelude qualified as P

-- Replace the first match in the vector with provided one.
replaceFirstMatchWith
  :: forall n a context
   . (Eq (a context), KnownNat n, Conditional (BooleanOf (a context)) (a context))
  => Vector n (a context)
  -- ^ Vector to replace the first match in.
  -> a context
  -- ^ Element to check for a match.
  -> a context
  -- ^ New element to replace the first match with.
  -> Vector n (a context)
  -- ^ Vector with the first match replaced.
replaceFirstMatchWith v match = replaceFirstMatchWith' v (== match)

-- Replace the first match (given the predicate function) in the vector with provided one.
replaceFirstMatchWith'
  :: forall n a context
   . (Eq (a context), KnownNat n, Conditional (BooleanOf (a context)) (a context))
  => Vector n (a context)
  -- ^ Vector to replace the first match in.
  -> (a context -> BooleanOf (a context))
  -- ^ Function to check if the element is a match.
  -> a context
  -- ^ New element to replace the first match with.
  -> Vector n (a context)
  -- ^ Vector with the first match replaced.
replaceFirstMatchWith' v mF new =
  -- The function definition is slightly contrived as we don't have normal Haskell equality for symbolic data types.
  let isMatch = mF P.<$> v
      prefixUsed = scanl (||) false isMatch
      usedBefore = take @n prefixUsed
      shouldIns = zipWith (\u m -> not u && m) usedBefore isMatch
   in mapWithIx (\ix old -> ifThenElse (shouldIns !! ix) new old) v

-- | Unsafe conversion from list to vector. This differs from `unsafeToVector` in that it throws an error if the list is not of the correct length.
unsafeToVector' :: forall size a. KnownNat size => HasCallStack => [a] -> Vector size a
unsafeToVector' as = case toVector as of
  P.Nothing -> P.error "unsafeToVector': toVector failed"
  P.Just v -> v
