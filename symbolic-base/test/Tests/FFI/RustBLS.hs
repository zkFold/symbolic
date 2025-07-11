{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Tests.FFI.RustBLS (specRustBLS) where

import GHC.IO (unsafePerformIO)
import Test.Hspec
import Test.QuickCheck hiding (scale)
import ZkFold.Algebra.Class
import qualified ZkFold.Algebra.EllipticCurve.BLS12_381 as Haskell
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import ZkFold.Algebra.Number (Natural)
import ZkFold.Algebra.Polynomial.Univariate
import qualified ZkFold.Data.Vector as V
import ZkFold.FFI.Rust.Conversion
import ZkFold.FFI.Rust.Types (RustPolyVec)
import qualified ZkFold.FFI.Rust.Types as Rust
import ZkFold.Protocol.NonInteractiveProof.Class ()
import Prelude (Eq (..), Integer, Show (..), String, abs, fromInteger, id, return, ($), (.), (<$>), (<>))

specRustBLS :: Spec
specRustBLS = do
  specScalar

type Length = 20

type Length2 = 2000

castHaskell :: PolyVec Haskell.Fr Length -> PolyVec Haskell.Fr Length2
castHaskell = castPolyVec @Haskell.Fr @(PolyVec Haskell.Fr) @Length @Length2

castRust :: RustPolyVec Rust.Fr Length -> RustPolyVec Rust.Fr Length2
castRust = castPolyVec @Rust.Fr @(RustPolyVec Rust.Fr) @Length @Length2

testConst :: forall h r. (Eq h, RustHaskell r h) => String -> h -> r -> Spec
testConst description h r = it ("should be correct:" <> description) $ do
  property $ h == r2h r

testUnary
  :: forall h1 h r1 r
   . (Show h1, Arbitrary h1, Eq h, RustHaskell r1 h1, RustHaskell r h)
  => String
  -> (h1 -> h)
  -> (r1 -> r)
  -> Spec
testUnary description h r = it ("should be correct: " <> description) $ do
  property $ \(a :: h1) -> (h a) == (r2h $ r $ h2r a)

testBinary
  :: forall h1 h2 h r1 r2 r
   . (Show h1, Show h2, Show h, Arbitrary h1, Arbitrary h2, Eq h, RustHaskell r1 h1, RustHaskell r2 h2, RustHaskell r h)
  => String
  -> (h1 -> h2 -> h)
  -> (r1 -> r2 -> r)
  -> Spec
testBinary description h r = it ("should be correct: " <> description) $ do
  property $ \(a :: h1) (b :: h2) -> h a b == r2h (r (h2r a) (h2r b))

specScalar :: Spec
specScalar = do
  describe "Rust BLS12-381 specification" $ do
    describe "Scalar" $ do
      testUnary @Haskell.Fr "conversion" id id

      testBinary @Haskell.Fr "add" (+) (+)
      testBinary @Haskell.Fr "sub" (-) (-)
      testBinary @Haskell.Fr "mul" (*) (*)
      testBinary @Haskell.Fr "div" (//) (//)

      testUnary @Haskell.Fr "negate" negate negate
      testUnary @Haskell.Fr "invert" finv finv

      testConst @Haskell.Fr "zero" zero zero
      testConst @Haskell.Fr "one" one one

      it "should be correct: scale natural" $ do
        property $ \(a :: Haskell.Fr) (b :: Natural) -> (scale b a) == r2h (scale b (h2r a))
      it "should be correct: from natural" $ do
        property $ \(a :: Integer) ->
          (fromConstant a :: PolyVec Haskell.Fr Length) == (r2h $ fromConstant a)
      it "should be correct: exp" $ do
        property $ \(a :: Haskell.Fr) (b :: Natural) ->
          (a ^ b)
            == r2h
              (h2r a ^ b)

    describe "Point G1" $ do
      testUnary @Haskell.BLS12_381_G1_Point "conversion" id id
      testBinary @Haskell.BLS12_381_G1_Point "add" (+) (+)
      testBinary @Haskell.Fr @Haskell.BLS12_381_G1_Point "scale" scale scale
      testBinary @Haskell.BLS12_381_G1_Point "sub" (-) (-)
      testUnary @Haskell.BLS12_381_G1_Point "negate" negate negate
      it "should be correct: scale natural" $ do
        property $ \(a :: Haskell.BLS12_381_G1_Point) (b :: Natural) -> (scale b a) == r2h (scale b (h2r a))
      testConst @Haskell.BLS12_381_G1_Point "zero" zero zero

      testConst @Haskell.BLS12_381_G1_Point "generator" pointGen pointGen

    describe "Point G2" $ do
      testUnary @Haskell.BLS12_381_G2_Point "conversion" id id
      testBinary @Haskell.BLS12_381_G2_Point "add" (+) (+)
      testBinary @Haskell.Fr @Haskell.BLS12_381_G2_Point "scale" scale scale
      testBinary @Haskell.BLS12_381_G2_Point "sub" (-) (-)
      testUnary @Haskell.BLS12_381_G2_Point "negate" negate negate
      it "should be correct: scale natural" $ do
        property $ \(a :: Haskell.BLS12_381_G2_Point) (b :: Natural) -> (scale b a) == r2h (scale b (h2r a))
      testConst @Haskell.BLS12_381_G2_Point "zero" zero zero
      testConst @Haskell.BLS12_381_G2_Point "generator" pointGen pointGen
    describe "MSM" $ do
      it "should be correct: msm" $ do
        withMaxSuccess 10 $ property $ \(scalars :: PolyVec Haskell.Fr 100) (points :: V.Vector 100 Haskell.BLS12_381_G1_Point) -> (bilinear (V.toV points) scalars :: Haskell.BLS12_381_G1_Point) == r2h (bilinear (h2r $ V.toV points) (h2r scalars))

    describe "Scalar Polynomial" $
      do
        testUnary @(PolyVec Haskell.Fr Length) "conversion" id id
        testBinary @(PolyVec Haskell.Fr Length) "add" (+) (+)
        testBinary @(PolyVec Haskell.Fr Length) "sub" (-) (-)
        testBinary @(PolyVec Haskell.Fr Length)
          "mul"
          (\x y -> (castHaskell x) * (castHaskell y))
          (\x y -> (castRust x) * (castRust y))

        testBinary @(PolyVec Haskell.Fr Length) "div" polyVecDiv polyVecDiv
        testBinary @(PolyVec Haskell.Fr Length) "hmul" (.*.) (.*.)
        testBinary @Haskell.Fr @(PolyVec Haskell.Fr Length) "mul on scalar" scale scale
        testBinary @Haskell.Fr @(PolyVec Haskell.Fr Length) "add scalar to coeffs" (+.) (+.)
        testUnary @(PolyVec Haskell.Fr Length) "negate" negate negate
        testConst @(PolyVec Haskell.Fr Length) "zero" zero zero
        testConst @(PolyVec Haskell.Fr Length) "one" one one
    describe "Natural" $ do
      testUnary @Natural "conversion" id id

      it "should be correct: exp" $ do
        property $ \(a :: PolyVec Haskell.Fr Length) (b :: Natural) ->
          ((castHaskell a) ^ b)
            == r2h
              ((castRust $ h2r a) ^ b)

      testBinary @(PolyVec Haskell.Fr Length) "hdiv" (./.) (./.)

      it "should be correct: scale natural" $ do
        property $ \(a :: PolyVec Haskell.Fr Length) (b :: Natural) ->
          (scale b (castHaskell a)) == r2h (scale b (castRust $ h2r a))

      it "should be correct: scale integer" $ do
        property $ \(a :: PolyVec Haskell.Fr Length) (b :: Integer) ->
          (scale b (castHaskell a)) == r2h (scale b (castRust $ h2r a))

      it "should be correct: from natural" $ do
        property $ \(a :: Natural) ->
          (fromConstant a :: PolyVec Haskell.Fr Length) == (r2h $ fromConstant a)

      it "should be correct: from natural" $ do
        property $ \(a :: Integer) ->
          (fromConstant a :: PolyVec Haskell.Fr Length) == (r2h $ fromConstant a)

-- describe "GT" $ do

-- testUnary @Haskell.BLS12_381_GT "conversion" id id

-- testBinary @Haskell.BLS12_381_G1_Point @Haskell.BLS12_381_G2_Point "pairing" pairing pairing

-- testBinary @Haskell.BLS12_381_GT @Haskell.Fr "exp" (^) (^)

-- testBinary @Haskell.BLS12_381_GT "mul" (*) (*)

instance Arbitrary Natural where
  arbitrary = fromInteger . abs <$> arbitrary

deriving newtype instance Arbitrary Haskell.BLS12_381_GT
