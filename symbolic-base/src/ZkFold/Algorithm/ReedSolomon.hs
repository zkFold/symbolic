{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Algorithm.ReedSolomon where

import Data.Bool (bool)
import Data.Vector as V hiding (sum)
import GHC.Natural (Natural)
import Prelude (
  Eq,
  Int,
  Integer,
  Maybe (..),
  error,
  fromIntegral,
  iterate,
  min,
  ($),
  (.),
  (<=),
  (==),
 )
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (KnownNat, value)
import ZkFold.Algebra.Polynomial.Univariate

numberOfError :: forall n k. (KnownNat k, KnownNat n) => Natural
numberOfError = (value @n -! value @k) `div` 2

generator :: (Eq a, Field a) => Int -> a -> Poly a
generator r a = V.foldr (\ai pi -> toLinPoly ai * pi) one roots
 where
  roots = V.iterateN r (* a) a
  toLinPoly p = toPoly $ fromList [negate p, one]

encode :: (Eq c, Field c) => [c] -> c -> Int -> Poly c
encode msg prim_elem r = msg_padded - reminder
 where
  g_x = generator r prim_elem
  poly_msg = toPoly $ fromList msg
  msg_padded = scaleP one (fromIntegral r) poly_msg
  (_, reminder) = qr msg_padded g_x

-- beta = one
decode :: forall c. (Eq c, Field c) => Poly c -> c -> Int -> Int -> Poly c
decode encoded primeElement r n = bool decoded encoded' isCorrect
 where
  rElems = iterateN r (* primeElement) primeElement

  encoded' = toPoly . V.drop r $ fromPoly encoded
  syndromes = toPoly . V.map (evalPoly encoded) $ rElems
  isCorrect = zero == syndromes

  (_, lx) = berlekamp syndromes r

  invPE = finv primeElement
  es1 = V.indexed . V.fromList $ one : P.take (n P.- 1) (iterate (* invPE) invPE)
  iroots = mapMaybe (\(i, x) -> bool Nothing (Just (i, x)) (evalPoly lx x == zero)) es1

  omega = toPoly @c @(Poly c) $ take r $ fromPoly (lx * syndromes)
  lx' = diff lx

  err =
    V.foldl (+) zero $
      map
        ( \(i, x) ->
            let xi = bool (monomial (fromIntegral i) one) (constant one) (i == 0)
                ei = evalPoly omega x * finv (evalPoly lx' x)
             in constant ei * xi
        )
        iroots

  fx = encoded + err
  checkSum = V.map (evalPoly fx) rElems

  decoded = bool (error "Can't decode") (toPoly $ V.drop r $ fromPoly fx) (all (== zero) checkSum)

berlekamp :: forall c. (Eq c, Field c) => Poly c -> Int -> (Integer, Poly c)
berlekamp s r
  | deg s == -1 = (0, one)
  | P.otherwise = go сx0 bx0 0 0 1 one
 where
  sv = fromPoly s
  сx0 = one :: Poly c
  bx0 = one :: Poly c
  lenS = fromIntegral r

  go :: Poly c -> Poly c -> Integer -> Integer -> Natural -> c -> (Integer, Poly c)
  go !cx !bx !n !l !m !b
    | n == lenS = bool (error "locators didn't find") (l, cx) (deg cx == l)
    | P.otherwise = bool innerChoice (go cx bx n' l (m + 1) b) (d == zero)
   where
    d = scalarN (fromIntegral n P.+ 1) (fromIntegral l P.+ 1) lxv sv

    lxv = fromPoly cx
    cx' = cx - fromConstant d * constant (finv b) * bx * monomial m one

    n' = n + 1
    innerChoice = bool (go cx' bx n' l (m + 1) b) (go cx' cx n' (n + 1 - l) 1 d) (2 * l <= n)

scalarN :: Semiring c => Int -> Int -> Vector c -> Vector c -> c
scalarN q l lv rv = bool (sum $ V.zipWith (*) lPadded rPadded) zero (min l (length lv) <= q P.- length rv)
 where
  lPadded = V.drop (q P.- V.length rv) lv
  rPadded = V.reverse $ V.take q rv

diff :: (Eq c, Field c) => Poly c -> Poly c
diff p = let cs = fromPoly p in toPoly $ V.tail $ V.imap (\i c -> scale (fromIntegral i :: Integer) c) cs
