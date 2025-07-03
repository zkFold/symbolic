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

reducable :: (Field c, Ord i, Ord j, Ring j) => Poly c i j -> Poly c i j -> Bool
reducable l r = dividable (snd $ lt l) (snd $ lt r)

-- TODO: refactor reduction methods so that they never applied to non-reducible polynomials

reduce
  :: (Eq coef, Field coef, Ord var, Ord pow, Ring pow)
  => Poly coef var pow
  -> Poly coef var pow
  -> Poly coef var pow
reduce l r =
  let (cl, ml) = lt l
      (cr, mr) = lt r
   in l - scaleM (cl // cr, ml / mr) r

reduceMany
  :: (Eq coef, Field coef, Ord var, Ord pow, Ring pow)
  => Poly coef var pow
  -> [Poly coef var pow]
  -> Poly coef var pow
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
  :: (Eq coef, Field coef, Ord var, Ord pow, Ring pow)
  => Poly coef var pow
  -> [Poly coef var pow]
  -> Poly coef var pow
fullReduceMany h fs =
  let h' = reduceMany h fs
   in if zeroP h'
        then h'
        else poly [lt h'] + fullReduceMany (h' - poly [lt h']) fs

systemReduce
  :: (Eq coef, Field coef, Ord var, Ord pow, Ring pow)
  => [Poly coef var pow]
  -> [Poly coef var pow]
systemReduce = foldr f []
 where
  f p ps =
    let p' = fullReduceMany p ps
     in bool ps (p' : ps) (not $ zeroP p')

data GroebnerParams coef var pow = GroebnerParams
  { groebnerMaxSteps :: Natural
  , groebnerSPolySelector :: Poly coef var pow -> Bool
  }

makeSPoly
  :: (Eq coef, Field coef, Ord var, Ord pow, Ring pow)
  => Poly coef var pow
  -> Poly coef var pow
  -> Poly coef var pow
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
  :: (Eq coef, Field coef, Ord var, Ord pow, Ring pow)
  => GroebnerParams coef var pow
  -> [Poly coef var pow]
  -> [Poly coef var pow]
groebnerStep GroebnerParams {..} ps =
  let n = length ps
      inds = [(i, j) | i <- [0 .. n -! 1], j <- [0 .. n -! 1], i < j]
      ss = map (\(i, j) -> makeSPoly (ps !! i) (ps !! j) `reduceMany` ps) inds
      ss' = filter (not . zeroP) ss
      ss'' = filter groebnerSPolySelector ss'
      ps' = sortBy (flip compare) (ss'' ++ ps)
   in systemReduce ps'

groebner
  :: (Eq coef, Field coef, Ord var, Ord pow, Ring pow)
  => GroebnerParams coef var pow
  -> [Poly coef var pow]
  -> [Poly coef var pow]
groebner GroebnerParams {..} ps =
  let step = groebnerStep GroebnerParams {..}
      go 0 ps' = ps'
      go n ps' = go (n -! 1) $ step ps'
   in go groebnerMaxSteps ps

verifyGroebner
  :: (Eq coef, Field coef, Ord var, Ord pow, Ring pow)
  => GroebnerParams coef var pow
  -> [Poly coef var pow]
  -> Poly coef var pow
  -> Bool
verifyGroebner params ps = zeroP . (`fullReduceMany` groebner params ps)
