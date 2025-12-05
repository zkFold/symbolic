module ZkFold.Algorithm.Interpolation where

import Data.Foldable1 (foldr1)
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (Maybe (..), mapMaybe)
import Data.Monoid ((<>))
import Data.Semialign (Semialign, alignWith)
import Data.These (These (..))
import Data.Tuple (fst, snd)
import qualified GHC.Num as P

import ZkFold.Algebra.Class

-- | Transpose collection, denoting missing values with 'Nothing'.
branches :: Semialign f => NonEmpty (f a) -> f (NonEmpty (Maybe a))
branches src =
  fmap extend $ foldr1 (alignWith merge) $ fmap (NE.singleton . Just) <$> src
 where
  merge (This l) = l
  merge (That r) = Nothing NE.<| r
  merge (These l r) = l <> r
  extend v =
    v `NE.appendList` L.replicate (NE.length src P.- NE.length v) Nothing

pushInterpolation :: Semialign f => NonEmpty (a, f b) -> f (NonEmpty (a, b))
pushInterpolation src = prune . NE.zip (fst <$> src) <$> branches (snd <$> src)
 where
  prune = NE.fromList . mapMaybe (\(a, m) -> (a,) <$> m) . NE.toList

-- | A list of pairs (ith element, all elements but the ith)
cuts :: NonEmpty a -> NonEmpty (a, [a])
cuts xs =
  NE.zipWith (\x i -> (x, NE.take i xs <> NE.drop (i P.+ 1) xs)) xs $
    NE.iterate (P.+ 1) 0

interpolate :: Field a => a -> NonEmpty (a, a) -> a
interpolate pt fs =
  let ks =
        -- indicators from interpolation polynomial
        [ product [pt - d | d <- ds] // product [c - d | d <- ds]
        | (c, ds) <- NE.toList $ cuts (fmap fst fs)
        ]
   in -- branches to interpolate between
      sum [k * f | ((_, f), k) <- L.zip (NE.toList fs) ks]
