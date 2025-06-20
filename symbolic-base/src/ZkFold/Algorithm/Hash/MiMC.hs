module ZkFold.Algorithm.Hash.MiMC (
  module ZkFold.Algorithm.Hash.MiMC,
  module ZkFold.Algorithm.Hash.MiMC.Constants,
) where

import Data.List (reverse)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (Maybe (..))
import Numeric.Natural (Natural)
import ZkFold.Algebra.Class
import ZkFold.Algorithm.Hash.MiMC.Constants
import Prelude (error)

{- | MiMC-2n/n (Feistel) hash function.
See https://eprint.iacr.org/2016/492.pdf, page 5
-}
mimcHash2 :: forall x. Ring x => [x] -> x -> x -> x -> x
mimcHash2 xs k = case nonEmpty (reverse xs) of
  Just cs -> go cs
  Nothing -> error "mimcHash: empty list"
 where
  go :: NonEmpty x -> x -> x -> x
  go (c :| cs) xL xR =
    let t5 = (xL + k + c) ^ (5 :: Natural) + xR
     in case nonEmpty cs of
          Just cs' -> go cs' t5 xL
          Nothing -> t5

mimcHashN :: Ring x => [x] -> x -> [x] -> x
mimcHashN xs k = go
 where
  go [] = mimcHash2 xs k zero zero
  go [z] = mimcHash2 xs k zero z
  go [zL, zR] = mimcHash2 xs k zL zR
  go (zL : zR : zs') = go (mimcHash2 xs k zL zR : zs')
