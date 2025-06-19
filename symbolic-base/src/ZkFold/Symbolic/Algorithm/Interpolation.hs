{-# LANGUAGE BlockArguments #-}

module ZkFold.Symbolic.Algorithm.Interpolation where

import           Control.Monad                (foldM, zipWithM)
import           Data.Function                (($), (.))
import           Data.Functor                 (fmap, (<$>))
import           Data.Functor.Rep             (Representable, index, tabulate)
import           Data.List                    (lookup, (++))
import           Data.List.NonEmpty           (NonEmpty, drop, iterate, take, toList, zipWith)
import           Data.Maybe                   (fromJust)
import           Data.Traversable             (Traversable, for, sequence)
import           Data.Tuple                   (fst, snd)
import           GHC.Generics                 (Par1 (..), type (:.:) (Comp1))
import qualified Prelude                      as P

import           ZkFold.Algebra.Class
import           ZkFold.Data.Package          (pack)
import           ZkFold.Symbolic.Class        (BaseField, Symbolic, symbolic2F)
import           ZkFold.Symbolic.MonadCircuit (newAssigned)

-- | A list of pairs (ith element, all elements but the ith)
cuts :: NonEmpty a -> [(a, [a])]
cuts = toList . \xs -> zipWith (\x i -> (x, take i xs ++ drop (i P.+ 1) xs)) xs
    $ iterate (P.+ 1) 0

interpolateW :: (Field w, Representable f) => NonEmpty (w, f w) -> w -> f w
interpolateW fs pt =
    -- | indicators from interpolation polynomial
    let ks = [ product [ pt - d | d <- ds ] // product [ c - d | d <- ds ]
             | (c, ds) <- cuts (fmap fst fs)
             ]
     in tabulate \r ->
        -- | branches to interpolate between
        let is = (`index` r) . snd <$> toList fs
         in sum [ i * k | (i, k) <- P.zip is ks ]

interpolation ::
    forall c f . (Symbolic c, Representable f, Traversable f) =>
    NonEmpty (BaseField c, c f) -> c Par1 -> c f
interpolation bs point = symbolic2F (pack $ Comp1 bs) point
    (\(Comp1 (Comp1 fs)) (Par1 x) -> fromJust . lookup x $ toList fs)
    \(Comp1 (Comp1 fs)) (Par1 x) -> do
        -- | indicators from interpolation polynomial
        ks <- for (cuts $ fmap fst fs) \(c, ds) -> do
            let dm = fromConstant . finv $ product [ c - d | d <- ds ]
                     -- ^ indicator denominator
            foldM (\p d -> newAssigned \w -> w p * (w x - fromConstant d)) dm ds

        sequence $ tabulate \r -> do
            -- | branches to interpolate between
            let is = (`index` r) . snd <$> toList fs
            -- | addends of interpolation polynomial
            js <- zipWithM (\i k -> newAssigned $ ($ i) * ($ k)) is ks
            let z = fromConstant (zero :: BaseField c)
            foldM (\s j -> newAssigned $ ($ s) + ($ j)) z js
