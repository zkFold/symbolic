module ZkFold.Algebra.Polynomial.Multivariate.Groebner where

import Data.Bool (bool)
import Data.List (sortBy)
import GHC.Natural (Natural)
import ZkFold.Algebra.Class
import ZkFold.Algebra.Polynomial.Multivariate.Internal
import ZkFold.Algebra.Polynomial.Multivariate.Monomial
import ZkFold.Prelude (length, (!!))
import Prelude hiding (
  Num (..),
  drop,
  lcm,
  length,
  sum,
  take,
  (!!),
  (/),
 )

reducable :: Polynomial c i j => Poly c i j -> Poly c i j -> Bool
reducable l r = dividable (snd $ lt l) (snd $ lt r)

-- TODO: refactor reduction methods so that they never applied to non-reducible polynomials

reduce
  :: forall c i j
   . (Ring j, Polynomial c i j)
  => Poly c i j
  -> Poly c i j
  -> Poly c i j
reduce l r =
  let (cl, ml) = lt l
      (cr, mr) = lt r
   in l - scaleM (cl // cr, ml / mr) r

reduceMany
  :: forall c i j
   . (Ring j, Polynomial c i j)
  => Poly c i j
  -> [Poly c i j]
  -> Poly c i j
reduceMany h fs = if reduced then reduceMany h' fs else h'
 where
  (h', reduced) = reduceStep h fs False
  reduceStep p (q : qs) r
    | zeroP p = (h, r)
    | otherwise =
        if reducable p q
          then (reduce p q, True)
          else reduceStep p qs r
  reduceStep p [] r = (p, r)

fullReduceMany
  :: forall c i j
   . (Ring j, Polynomial c i j)
  => Poly c i j
  -> [Poly c i j]
  -> Poly c i j
fullReduceMany h fs =
  let h' = reduceMany h fs
   in if zeroP h'
        then h'
        else poly [lt h'] + fullReduceMany (h' - poly [lt h']) fs

systemReduce
  :: forall c i j
   . (Ring j, Polynomial c i j)
  => [Poly c i j]
  -> [Poly c i j]
systemReduce = foldr f []
 where
  f p ps =
    let p' = fullReduceMany p ps
     in bool ps (p' : ps) (not $ zeroP p')

data GroebnerParams c i j = GroebnerParams
  { groebnerMaxSteps :: Natural
  , groebnerSPolySelector :: Poly c i j -> Bool
  }

makeSPoly
  :: (Ring j, Polynomial c i j)
  => Poly c i j
  -> Poly c i j
  -> Poly c i j
makeSPoly l r =
  let (cl, ml) = lt l
      (cr, mr) = lt r

      m = gcdM ml mr
      lcm = lcmM ml mr

      ra = lcm / ml
      la = lcm / mr

      l' = (cr, ra) `scaleM` l
      r' = (cl, la) `scaleM` r
   in if oneM m
        then zero
        else r' - l'

groebnerStep
  :: (Ring j, Polynomial c i j)
  => GroebnerParams c i j
  -> [Poly c i j]
  -> [Poly c i j]
groebnerStep GroebnerParams {..} ps =
  let n = length ps
      inds = [(i, j) | i <- [0 .. n -! 1], j <- [0 .. n -! 1], i < j]
      ss = map (\(i, j) -> makeSPoly (ps !! i) (ps !! j) `reduceMany` ps) inds
      ss' = filter (not . zeroP) ss
      ss'' = filter groebnerSPolySelector ss'
      ps' = sortBy (flip compare) (ss'' ++ ps)
   in systemReduce ps'

groebner
  :: forall c i j
   . (Ring j, Polynomial c i j)
  => GroebnerParams c i j
  -> [Poly c i j]
  -> [Poly c i j]
groebner GroebnerParams {..} ps =
  let step = groebnerStep GroebnerParams {..}
      go 0 ps' = ps'
      go n ps' = go (n -! 1) $ step ps'
   in go groebnerMaxSteps ps

verifyGroebner
  :: forall c i j
   . (Ring j, Polynomial c i j)
  => GroebnerParams c i j
  -> [Poly c i j]
  -> Poly c i j
  -> Bool
verifyGroebner params ps = zeroP . (`fullReduceMany` groebner params ps)
