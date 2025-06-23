{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM)
import qualified Data.Vector as V
import System.Random (randomIO)
import Test.Tasty.Bench
import Prelude hiding (sum, (*), (+), (-), (/), (^))
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.Field
import ZkFold.Algebra.Number (Prime)
import ZkFold.Algebra.Polynomial.Univariate

-- | Only for testing DFT with smaller numbers which can be easily calculated by hand for cross-check.
-- DFT of a polynomial of length n requires calculating primitive roots of unity of order n.
-- Choosing 257 allows us to calculate DFT of polynomials of length up to 256 as all these numbers divide 257 - 1.
type TestPrime = 257

-- | Generate random polynomials of given size
polynomials :: forall a. Prime a => Int -> IO (Poly (Zp a), Poly (Zp a))
polynomials size = do
  coeffs1 <- replicateM size (toZp @a <$> randomIO)
  coeffs2 <- replicateM size (toZp @a <$> randomIO)
  evaluatedCoeffs1 <- evaluate . force . V.fromList $ coeffs1
  evaluatedCoeffs2 <- evaluate . force . V.fromList $ coeffs2
  pure (toPoly evaluatedCoeffs1, toPoly evaluatedCoeffs2)

sizes :: [Int]
sizes = [1, 2, 3] <> (((4 :: Int) P.^) <$> [1 .. 5 :: Int]) <> (((2 :: Int) P.^) <$> [11 .. 13 :: Int])

ops :: (Eq a, Field a) => [(String, Poly a -> Poly a -> Poly a)]
ops =
  [ ("DFT multiplication", mulPolyDft)
  , ("Adaptive multiplication", (*))
  , ("Karatsuba multiplication", mulPolyKaratsuba)
  , ("Vector multiplication", mulPoly)
  , ("Naive multiplication", mulPolyNaive)
  ]

benchOps :: Prime a => Int -> [(String, Poly (Zp a) -> Poly (Zp a) -> Poly (Zp a))] -> Benchmark
benchOps size testOps = env (polynomials size) $ \ ~(p1, p2) ->
  bgroup ("Multiplying polynomials of size " <> show size) $
    flip fmap testOps $
      \(desc, op) -> bench desc $ nf (uncurry op) (p1, p2)

main :: IO ()
main = do
  forM_ sizes $ \s -> do
    (p1, p2) <- polynomials @BLS12_381_Scalar s
    putStrLn $ "Size " <> show s
    let ref = p1 `mulPolyNaive` p2
    putStrLn $ "Karatsuba\t" <> show (ref == p1 `mulPolyKaratsuba` p2)
    putStrLn $ "Vector\t\t" <> show (ref == p1 `mulPoly` p2)
    putStrLn $ "DFT\t\t" <> show (ref == p1 `mulPolyDft` p2)
  defaultMain
    [ bgroup "Field with roots of unity" $ flip fmap sizes $ \s -> benchOps @BLS12_381_Scalar s ops
    , bgroup "Field with roots of unity up to 256" $ flip fmap sizes $ \s -> benchOps @TestPrime s $ tail ops
    , bgroup "Field without roots of unity" $ flip fmap sizes $ \s -> benchOps @BLS12_381_Base s $ tail ops
    ]
