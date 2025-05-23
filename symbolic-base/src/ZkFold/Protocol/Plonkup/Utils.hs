{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Utils where

import           Data.Bool                          (bool)
import           Data.List                          (sortOn)
import qualified Data.Map                           as M
import qualified Data.Set                           as S
import           Prelude                            hiding (Num (..), drop, length, replicate, sum, take, (!!), (/),
                                                     (^))

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import           ZkFold.Algebra.Number
import           ZkFold.Data.Vector                 (Vector, unsafeToVector)
import           ZkFold.Prelude                     (iterateN', log2ceiling)
import           ZkFold.Symbolic.Class              (Arithmetic)

getParams :: forall a . (Ord a, FiniteField a) => Natural -> (a, a, a)
getParams n = findK' 0
    where
        omega = case rootOfUnity @a (log2ceiling n) of
                  Just o -> o
                  _      -> error "impossible"

        hGroup = iterateN' n (*omega) one

        hGroupS = S.fromList hGroup
        hGroup' k = S.fromList $ map (k*) hGroup


        findK' :: Natural -> (a, a, a)
        findK' g =
            let k1 = fromConstant g
                k2 = fromConstant (g + 42)
                hGroupK1 = hGroup' k1
                hGroupK2 = hGroup' k2
            in bool (findK' (g * 3)) (omega, k1, k2) $
                   S.disjoint hGroupS hGroupK1
                && S.disjoint hGroupK1 hGroupK2

getSecretParams :: forall n g1 g2 .
    ( KnownNat n
    , Arithmetic (ScalarFieldOf g1)
    , CyclicGroup g1
    , CyclicGroup g2
    , Scale (ScalarFieldOf g1) g2
    ) => ScalarFieldOf g1 -> (Vector (n + 5) g1, g2)
getSecretParams x =
    let xs = unsafeToVector $ fmap (x^) [0 .. (value @n + 5)]
        gs = fmap (`scale` pointGen) xs
        h1 = x `scale` pointGen
    in (gs, h1)

sortByList :: Ord a => [a] -> [a] -> [a]
-- ^ Given two lists @l1@ and @l2@,
-- sorts elements from @l1@ according to the order specified in @l2@.
--
-- Preconditions:
-- * @all (`elem` l2) l1@
-- * @map head (group l2) == nub l2@
sortByList f t =
    let ixMap = M.fromList (zip t [(0 :: Int)..])
     in sortOn (ixMap M.!) f
