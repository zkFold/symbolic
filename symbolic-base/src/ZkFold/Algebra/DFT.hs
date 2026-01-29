{-# LANGUAGE BlockArguments #-}

module ZkFold.Algebra.DFT (genericDft) where

import Data.Bits ((.<<.), (.>>.), (.|.))
import Data.Foldable (for_)
import qualified Data.STRef as ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Prelude hiding (mod, sum, (*), (+), (-), (/), (^))
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number

-- | Generic FFT algorithm. Can be both direct and inverse
-- depending on @wn@ (root of unity or its inverse) supplied.
-- Does not apply scaling when it's inverse.
-- Requires the vector to be of length 2^@n@.
-- Implementation: iterative radix-2 FFT algorithm
-- implemented using bit-reversal permutation (in-place).
genericDft
  :: forall a
   . Ring a
  => Int
  -> a
  -> V.Vector a
  -> V.Vector a
genericDft 0 _ !v = v
genericDft !n !wn !v = V.create $ do
  !result <- V.thaw (bitReversalPermutation v)
  !wRef <- ST.newSTRef one

  for_ [1 .. n] \s -> do
    let !m = 2 P.^ s
        !wm = wn ^ (2 P.^ (n P.- s) :: Natural)
    for_ [0, m .. len P.- 1] \k -> do
      ST.writeSTRef wRef one
      for_ [0 .. (m `P.div` 2) P.- 1] \j -> do
        let !iu = k P.+ j
            !it = iu P.+ (m `P.div` 2)

        !w <- ST.readSTRef wRef
        !t <- (w *) <$> VM.unsafeRead result it
        !u <- VM.unsafeRead result iu
        VM.unsafeWrite result iu $ u + t
        VM.unsafeWrite result it $ u - t

        ST.modifySTRef wRef (* wm)
  pure result
 where
  !len = P.fromIntegral (2 P.^ n :: Natural)

-- | Bit-reversal permutation is a permutation of a sequence of n items, where n = 2^k.
-- It is defined by indexing the elements of the sequence by the numbers from 0 to n − 1,
-- representing each of these numbers by its binary representation (padded to have length exactly k),
-- and mapping each item to the item whose representation has the same bits in the reversed order.
bitReversalPermutation :: forall a. V.Vector a -> V.Vector a
bitReversalPermutation !v = V.generate n \i -> v V.! reverseBits n2 i
 where
  n = V.length v
  n2 = n .>>. 1

reverseBits :: Int -> Int -> Int
reverseBits = go 0
 where
  go :: Int -> Int -> Int -> Int
  go !acc 0 _ = acc
  go !acc !b !n =
    let (d, m) = n `P.divMod` 2
     in go ((acc .<<. 1) .|. m) (b .>>. 1) d
