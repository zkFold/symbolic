{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Control.DeepSeq                             (NFData, force)
import           Control.Exception                           (evaluate)
import           Prelude                                     hiding (sum, (*), (+), (-), (/), (^))
import qualified Prelude                                     as P
import           Test.QuickCheck.Arbitrary                   (Arbitrary (..))
import           Test.QuickCheck.Gen                         (generate)
import           Test.Tasty.Bench

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Field
import           ZkFold.Algebra.Number
import           ZkFold.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Algebra.Polynomial.Univariate
import           ZkFold.Prelude                              (log2ceiling)


type F = Zp BLS12_381_Scalar

-- | Generate random polynomials of given size
--
polynomials :: forall n c . (KnownNat n, Ring c, Arbitrary c, NFData c)  => IO (PolyVec c n)
polynomials = do
    poly <- generate arbitrary
    evaluate . force $ poly

benchOps :: forall n s. (KnownNat n, KnownNat s) => Benchmark
benchOps = env (polynomials @n @F) $ \ ~p ->
    bgroup ("Polynomial of degree " <> P.show (value @n) <> " in Lagrange Basis of dimension " <> P.show (value @s)) $
        [ bench "polyVecInLagrangeBasis" $ nf (uncurry (polyVecInLagrangeBasis @F @(PolyVec F) @n @s)) (omega, p) ]
    where
        omega = case rootOfUnity @F (log2ceiling $ value @n) of
                  Just w  -> w
                  Nothing -> error "No roots of unity"


main :: IO ()
main = defaultMain
      [ benchOps @8 @8
      , benchOps @64 @128
      , benchOps @256 @128
      , benchOps @1024 @128
      , benchOps @4096 @128
      , benchOps @128 @64
      , benchOps @128 @256
      , benchOps @128 @1024
      , benchOps @128 @4096
      , benchOps @1024 @1024
      , benchOps @4096 @4096
      , benchOps @1048576 @1048576
      ]
