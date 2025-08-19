{-# LANGUAGE BlockArguments #-}

module ZkFold.Symbolic.Algorithm.Interpolation where

import Control.Monad (foldM, zipWithM)
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>), (<&>))
import Data.List (lookup, replicate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, Maybe (..), fromMaybe, mapMaybe)
import Data.Traversable (Traversable, for)
import Data.Tuple (fst, snd)
import GHC.Generics (Par1 (..), type (:.:) (Comp1))
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Data.Package (pack)
import ZkFold.Symbolic.Class (BaseField, Symbolic, symbolic2F)
import ZkFold.Symbolic.MonadCircuit (newAssigned)
import Data.Semialign (Semialign, alignWith, zipWith, zip)
import Data.Foldable (foldr1, toList, length)
import Data.These (These(..))
import Data.Semigroup ((<>))

-- | A list of pairs (ith element, all elements but the ith)
cuts :: NonEmpty a -> NonEmpty (a, [a])
cuts xs =
  zipWith (\x i -> (x, NE.take i xs <> NE.drop (i P.+ 1) xs)) xs $
    NE.iterate (P.+ 1) 0

-- | Transpose collection, denoting missing values with 'Nothing'.
branches :: Semialign f => NonEmpty (f a) -> f (NonEmpty (Maybe a))
branches src =
  fmap extend $ foldr1 (alignWith merge) $ fmap (NE.singleton . Just) <$> src
  where
    merge (This l) = l
    merge (That r) = NE.singleton Nothing <> r
    merge (These l r) = l <> r
    extend v = v `NE.appendList` replicate (length src P.- length v) Nothing

pushInterpolation :: Semialign f => NonEmpty (a, f b) -> f (NonEmpty (a, b))
pushInterpolation src = prune . zip (fst <$> src) <$> branches (snd <$> src)
  where prune = NE.fromList . mapMaybe (\(a, m) -> (a,) <$> m) . toList

interpolateW :: (Field w, Semialign f) => NonEmpty (w, f w) -> w -> f w
interpolateW fs pt =
  let ks = -- indicators from interpolation polynomial
        [ product [pt - d | d <- ds] // product [c - d | d <- ds]
        | (c, ds) <- toList $ cuts (fmap fst fs)
        ]
   in branches (fmap snd fs) <&> \is -> -- branches to interpolate between
        sum [i * k | (fromMaybe zero -> i, k) <- zip (toList is) ks]

interpolate
  :: forall c f
   . (Symbolic c, Semialign f, Traversable f)
  => NonEmpty (BaseField c, c f) -> c Par1 -> c f
interpolate bs point = symbolic2F
  (pack $ Comp1 bs)
  point
  (\(Comp1 (Comp1 fs)) (Par1 x) -> fromJust . lookup x $ toList fs)
  \(Comp1 (Comp1 fs)) (Par1 x) -> do
    -- indicators from interpolation polynomial
    ks <- for (toList $ cuts $ fmap fst fs) \(c, ds) -> do
      let dm = fromConstant . finv $ product [c - d | d <- ds] -- indicator denominator
      foldM (\p d -> newAssigned \w -> w p * (w x - fromConstant d)) dm ds
    for (branches $ fmap snd fs) \is -> do -- branches to interpolate between
        let z = fromConstant (zero :: BaseField c)
        -- addends of interpolation polynomial
        js <- zipWithM (\(fromMaybe z -> i) k -> newAssigned $ ($ i) * ($ k))
                       (toList is) ks
        foldM (\s j -> newAssigned $ ($ s) + ($ j)) z js
