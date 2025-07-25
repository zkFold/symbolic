{-# LANGUAGE TypeApplications #-}

module ZkFold.Algebra.Permutation (
  IndexSet,
  IndexPartition,
  Permutation,
  fromPermutation,
  applyPermutation,
  mkIndexPartition,
  fromCycles,
) where

import Control.DeepSeq (NFData)
import Control.Monad (forM_)
import Data.Aeson (ToJSON)
import Data.Functor.Rep (Representable (index))
import Data.Map.Strict (Map, elems, empty, insertWith)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Prelude hiding (Num (..), drop, length, mod, (!!))
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Data.Vector (Vector (..), unsafeToVector)
import ZkFold.Prelude (chooseNatural, drop, length, (!!))

-- TODO (Issue #18): make the code safer

------------------------------ Index sets and partitions -------------------------------------

type IndexSet = V.Vector Natural

type IndexPartition a = Map a IndexSet

mkIndexPartition :: Ord a => V.Vector a -> IndexPartition a
mkIndexPartition vs =
  fmap V.fromList $
    V.foldl' (\m (e, ix) -> insertWith (<>) e [ix] m) empty $
      V.zip vs (V.fromList [1 .. fromIntegral $ V.length vs])

------------------------------------- Permutations -------------------------------------------

newtype Permutation n = Permutation (Vector n Natural)
  deriving (Eq, Generic, NFData, Show, ToJSON)

instance KnownNat n => Arbitrary (Permutation n) where
  arbitrary =
    let f as [] = return as
        f as bs = do
          i <- chooseNatural (0, length bs -! 1)
          let as' = (bs !! i) : as
              bs' = drop i bs
          f as' bs'
     in Permutation . unsafeToVector
          <$> f [] [fromConstant x | x <- [1 .. value @n]]

fromPermutation :: Permutation n -> V.Vector Natural
fromPermutation (Permutation perm) = toV perm

applyPermutation :: KnownNat n => Permutation n -> Vector n a -> Vector n a
applyPermutation (Permutation ps) as =
  let
    -- 1-indexed Natural to 0-indexed Zp n
    naturalToZp ix = fromConstant ix - 1
   in
    fmap (index as . naturalToZp) ps

fromCycles :: forall n a. IndexPartition a -> Permutation n
fromCycles p = Permutation . Vector $ V.create $ do
  ixes <- VM.generate (n P.+ 1) id -- "reverse" array, returns index by element
  forM_ lists $ \cyc -> do
    pos0 <- VM.read ixes (P.fromIntegral $ V.last cyc)
    let cycLen = V.length cyc

    -- ixes[cyc[cix + 1]] := ixes[cyc[cix]];
    -- the position of the next element in the cycle becomes the position of the current one
    forM_ [cycLen P.- 2, cycLen P.- 3 .. 0] $ \cix -> do
      !pos <- VM.read ixes (P.fromIntegral $ cyc V.! cix)
      VM.write ixes (P.fromIntegral $ cyc V.! (cix P.+ 1)) pos
    VM.write ixes (P.fromIntegral $ V.head cyc) pos0

  frozenIxes <- V.unsafeFreeze ixes -- we're not going to modify @ixes@ anymore, so O(1) @unsafeFreeze@ is ok
  res <- VM.new n
  forM_ [1 .. n] $ \ix -> VM.write res ((frozenIxes V.! ix) P.- 1) (fromIntegral ix :: Natural)
  pure res
 where
  lists :: V.Vector (V.Vector Natural)
  !lists = V.reverse . V.fromList $ elems p

  n :: P.Int
  n = P.sum $ V.length <$> lists
