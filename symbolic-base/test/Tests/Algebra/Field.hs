{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use -"                  #-}

module Tests.Algebra.Field (specField) where

import Data.Data (Typeable, typeOf)
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (Fractional (..), Num (..), length)

import ZkFold.Algebra.Class
import qualified ZkFold.Algebra.EllipticCurve.BLS12_381 as BLS12_381
import qualified ZkFold.Algebra.EllipticCurve.BN254 as BN254
import qualified ZkFold.Algebra.EllipticCurve.Pasta as Pasta

specField' :: forall a. (Arbitrary a, Eq a, Field a, Show a, Typeable a) => Spec
specField' = do
  describe "Field specification" $ do
    describe ("Type: " ++ show (typeOf @a zero)) $ do
      describe "Field axioms" $ do
        it "should satisfy additive associativity" $ do
          property $ \(a :: a) b c -> (a + b) + c == a + (b + c)
        it "should satisfy additive commutativity" $ do
          property $ \(a :: a) b -> a + b == b + a
        it "should satisfy additive identity" $ do
          property $ \(a :: a) -> a + zero == a
        it "should satisfy additive inverse" $ do
          property $ \(a :: a) -> a + negate a == zero
        it "should satisfy multiplicative associativity" $ do
          property $ \(a :: a) b c -> (a * b) * c == a * (b * c)
        it "should satisfy multiplicative commutativity" $ do
          property $ \(a :: a) b -> a * b == b * a
        it "should satisfy multiplicative identity" $ do
          property $ \(a :: a) -> a * one == a
        it "should satisfy multiplicative inverse" $ do
          property $ \(a :: a) -> a /= zero ==> a * finv a == one
        it "should satisfy distributivity" $ do
          property $ \(a :: a) b c -> a * (b + c) == a * b + a * c

specField :: Spec
specField = do
  specField' @BN254.Fr
  specField' @BN254.Fp
  specField' @BN254.Fp2
  specField' @BN254.Fp6
  specField' @BN254.Fp12

  specField' @BLS12_381.Fr
  specField' @BLS12_381.Fq
  specField' @BLS12_381.Fq2
  specField' @BLS12_381.Fq6
  specField' @BLS12_381.Fq12

  specField' @Pasta.Fp
  specField' @Pasta.Fq
