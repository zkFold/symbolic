{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}

module ZkFold.Algebra.Permutation (
    IndexSet,
    IndexPartition,
    Permutation,
    fromPermutation,
    applyPermutation,
    mkIndexPartition,
    fromCycles
) where

import           Control.DeepSeq       (NFData, force)
import           Data.Functor.Rep      (Representable (index))
import           Data.Map.Strict       (Map, elems, empty, insertWith)
import           Data.Maybe            (fromJust)
import qualified Data.Vector           as V
import           GHC.Generics          (Generic)
import           Prelude               hiding (Num (..), drop, length, mod, (!!))
import qualified Prelude               as P
import           Test.QuickCheck       (Arbitrary (..))

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Number
import           ZkFold.Data.Vector    (Vector (..), toVector, unsafeToVector)
import           ZkFold.Prelude        (chooseNatural, drop, length, (!!))

-- TODO (Issue #18): make the code safer

------------------------------ Index sets and partitions -------------------------------------

type IndexSet = V.Vector Natural
type IndexPartition a = Map a IndexSet

mkIndexPartition :: Ord a => V.Vector a -> IndexPartition a
mkIndexPartition vs = fmap V.fromList $ V.foldl' (\m (e, ix) -> insertWith (<>) e [ix] m) empty $ V.zip vs [1 .. fromIntegral $ V.length vs]

------------------------------------- Permutations -------------------------------------------

newtype Permutation n = Permutation (Vector n Natural)
    deriving (Show, Eq, Generic, NFData)

instance KnownNat n => Arbitrary (Permutation n) where
    arbitrary =
        let f as [] = return as
            f as bs = do
                i <- chooseNatural (0, length bs -! 1)
                let as' = (bs !! i) : as
                    bs' = drop i bs
                f as' bs'
        in Permutation . unsafeToVector <$>
              f [] [fromConstant x | x <- [1..value @n]]

fromPermutation :: Permutation n -> V.Vector Natural
fromPermutation (Permutation perm) = toV perm

applyPermutation :: KnownNat n => Permutation n -> Vector n a -> Vector n a
applyPermutation (Permutation ps) as =
    let
        -- 1-indexed Natural to 0-indexed Zp n
        naturalToZp ix = fromConstant ix - 1
    in
        fmap (index as . naturalToZp) ps

applyCycle :: V.Vector Natural -> Permutation n -> Permutation n
applyCycle c (Permutation perm) = force $ Permutation $ fmap f perm
    where
        f :: Natural -> Natural
        f i = case i `V.elemIndex` c of
            Just j  -> c V.! ((j P.+ 1) `P.mod` V.length c)
            Nothing -> i

fromCycles :: KnownNat n => IndexPartition a -> Permutation n
fromCycles p =
    let n = fromIntegral $ V.length $ V.concat $ elems p
    in foldr applyCycle (Permutation $ fromJust $ toVector [1 .. n]) $ elems p

