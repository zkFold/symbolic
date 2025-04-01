{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Protocol.Plonkup.Utils where

import           Data.Bifunctor                          (first)
import           Data.Bool                               (bool)
import           Data.Map                                (fromList, insertWith, toList)
import qualified Data.Set                                as S
import           Prelude                                 hiding (Num (..), drop, length, replicate, sum, take, (!!),
                                                          (/), (^))
import           System.Random                           (RandomGen, mkStdGen, uniformR)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.Class (CyclicGroup (..))
import           ZkFold.Base.Data.Vector                 (Vector, unsafeToVector)
import           ZkFold.Prelude                          (log2ceiling, replicate, iterateN)
import           ZkFold.Symbolic.Class                   (Arithmetic)

getParams :: forall a . (Ord a, FiniteField a) => Natural -> (a, a, a)
getParams n = findK' $ mkStdGen 0
    where
        omega = case rootOfUnity @a (log2ceiling n) of
                  Just o -> o
                  _      -> error "impossible"

        hGroup = iterateN (*omega) (n -! 1) one

        hGroupS = S.fromList hGroup
        hGroup' k = S.fromList $ map (k*) hGroup

        findK' :: RandomGen g => g -> (a, a, a)
        findK' g =
            let (k1, g') = first fromConstant $ uniformR (1, order @a -! 1) g
                (k2, g'') = first fromConstant $ uniformR (1, order @a -! 1) g'
                hGroupK1 = hGroup' k1
                hGroupK2 = hGroup' k2
            in bool (findK' g'') (omega, k1, k2) $
                   S.disjoint hGroupS hGroupK1
                && S.disjoint hGroupK1 hGroupK2

getSecrectParams :: forall n g1 g2 .
    ( KnownNat n
    , Arithmetic (ScalarFieldOf g1)
    , CyclicGroup g1
    , CyclicGroup g2
    , Scale (ScalarFieldOf g1) g2
    ) => ScalarFieldOf g1 -> (Vector (n + 5) g1, g2)
getSecrectParams x =
    let xs = unsafeToVector $ fmap (x^) [0 .. (value @n + 5)]
        gs = fmap (`scale` pointGen) xs
        h1 = x `scale` pointGen
    in (gs, h1)

sortByList :: Ord a => [a] -> [a] -> [a]
sortByList f t =
    let m  = fromList $ zip t (repeat @Natural 0)
        m' = foldl (\acc x -> insertWith (+) x 1 acc) m f
    in concatMap (\(k, v) -> replicate v k) $ toList m'
