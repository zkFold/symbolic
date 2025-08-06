{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Tests.FFI.RustBLS (specRustBLS) where

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
import Prelude (Eq (..), Show, Integer, String, abs, fromInteger, id, ($), (.), (<>))
import Control.Applicative

specRustBLS :: Spec
specRustBLS = do
  specScalar

type Length = 20

type Length2 = 2000

castHaskell :: PolyVec Haskell.Fr Length -> PolyVec Haskell.Fr Length2
castHaskell = castPolyVec @Haskell.Fr @(PolyVec Haskell.Fr) @Length @Length2

castRust :: RustPolyVec Rust.Fr Length -> RustPolyVec Rust.Fr Length2
castRust = castPolyVec @Rust.Fr @(RustPolyVec Rust.Fr) @Length @Length2

class TestRustHaskell r h where
  h2r' :: h -> r
  r2h' :: r -> h

instance {-# OVERLAPPABLE #-} (RustHaskell r h) => TestRustHaskell r h where
  h2r' = h2r
  r2h' = r2h

instance TestRustHaskell Natural Natural where
  h2r' = id
  r2h' = id

instance TestRustHaskell Integer Integer where
  h2r' = id
  r2h' = id

testConst :: forall h r. (Eq h, TestRustHaskell r h) => String -> h -> r -> Spec
testConst description h r = it ("should be correct:" <> description) $ do
  property $ h == r2h' r

testUnary
  :: forall h1 h r1 r
   . (Show h1, Arbitrary h1, Eq h, TestRustHaskell r1 h1, TestRustHaskell r h)
  => String
  -> (h1 -> h)
  -> (r1 -> r)
  -> Spec
testUnary description h r = it ("should be correct: " <> description) $ do
  property $ \(a :: h1) -> h a == r2h' (r $ h2r' a)

testBinary
  :: forall h1 h2 h r1 r2 r
   . (Show h1, Show h2, Arbitrary h1, Arbitrary h2, Eq h, TestRustHaskell r1 h1, TestRustHaskell r2 h2, TestRustHaskell r h)
  => String
  -> (h1 -> h2 -> h)
  -> (r1 -> r2 -> r)
  -> Spec
testBinary description h r = it ("should be correct: " <> description) $ do
  property $ \(a :: h1) (b :: h2) -> h a b == r2h' (r (h2r' a) (h2r' b))

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

      testBinary @Natural @Haskell.Fr @_ @Natural "scale natural" scale scale
      testBinary @Integer @Haskell.Fr @_ @Integer "scale integer" scale scale

      testUnary @Natural @Haskell.Fr @Natural "from natural" fromConstant fromConstant
      testUnary @Integer @Haskell.Fr @Integer "from integer" fromConstant fromConstant

      testBinary @Haskell.Fr @Natural @_ @_ @Natural "exp" (^) (^)
      testBinary @Haskell.Fr @Integer @_ @_ @Integer "exp" (^) (^)

    describe "Point G1" $ do
      testUnary @Haskell.BLS12_381_G1_Point "conversion" id id

      testBinary @Haskell.BLS12_381_G1_Point "add" (+) (+)
      testBinary @Haskell.BLS12_381_G1_Point "sub" (-) (-)

      testUnary @Haskell.BLS12_381_G1_Point "negate" negate negate

      testConst @Haskell.BLS12_381_G1_Point "zero" zero zero

      testBinary @Haskell.Fr @Haskell.BLS12_381_G1_Point "scale" scale scale

      testBinary @Natural @Haskell.BLS12_381_G1_Point @_ @Natural "scale natural" scale scale
      testBinary @Integer @Haskell.BLS12_381_G1_Point @_ @Integer "scale integer" scale scale

      testConst @Haskell.BLS12_381_G1_Point "generator" pointGen pointGen

    describe "Point G2" $ do
      testUnary @Haskell.BLS12_381_G2_Point "conversion" id id

      testBinary @Haskell.BLS12_381_G2_Point "add" (+) (+)
      testBinary @Haskell.BLS12_381_G2_Point "sub" (-) (-)

      testUnary @Haskell.BLS12_381_G2_Point "negate" negate negate

      testConst @Haskell.BLS12_381_G2_Point "zero" zero zero

      testBinary @Haskell.Fr @Haskell.BLS12_381_G2_Point "scale" scale scale

      testBinary @Natural @Haskell.BLS12_381_G2_Point @_ @Natural "scale natural" scale scale
      testBinary @Integer @Haskell.BLS12_381_G2_Point @_ @Integer "scale integer" scale scale

      testConst @Haskell.BLS12_381_G2_Point "generator" pointGen pointGen

    describe "MSM" $ do
      it "should be correct: msm" $ do
        withMaxSuccess 10 $ property $ 
          \(scalars :: PolyVec Haskell.Fr 100) (points :: V.Vector 100 Haskell.BLS12_381_G1_Point) 
            -> (bilinear (V.toV points) scalars :: Haskell.BLS12_381_G1_Point) 
            == r2h (bilinear (h2r $ V.toV points) (h2r scalars))

    describe "Scalar Polynomial" $ do
      testUnary @(PolyVec Haskell.Fr Length) "conversion" id id

      testBinary @(PolyVec Haskell.Fr Length) "add" (+) (+)
      testBinary @(PolyVec Haskell.Fr Length) "sub" (-) (-)
      testBinary @(PolyVec Haskell.Fr Length) "mul"
        (\x y -> (castHaskell x) * (castHaskell y))
        (\x y -> (castRust x) * (castRust y))
      testBinary @(PolyVec Haskell.Fr Length) "div" polyVecDiv polyVecDiv
      
      testUnary @(PolyVec Haskell.Fr Length) "negate" negate negate

      testConst @(PolyVec Haskell.Fr Length) "zero" zero zero
      testConst @(PolyVec Haskell.Fr Length) "one" one one

      testBinary @Haskell.Fr @(PolyVec Haskell.Fr Length) "mul on scalar" scale scale

      testBinary @Natural @(PolyVec Haskell.Fr Length) @_ @Natural "scale natural" scale scale
      testBinary @Integer @(PolyVec Haskell.Fr Length) @_ @Integer "scale integer" scale scale

      testUnary @Natural @(PolyVec Haskell.Fr Length) @Natural "from natural" fromConstant fromConstant
      testUnary @Integer @(PolyVec Haskell.Fr Length) @Integer "from integer" fromConstant fromConstant

      testBinary @(PolyVec Haskell.Fr Length) @Natural @_ @_ @Natural "exp" ((^) . castHaskell) ((^) . castRust)

      testBinary @(PolyVec Haskell.Fr Length) "hmul" (.*.) (.*.)
      testBinary @Haskell.Fr @(PolyVec Haskell.Fr Length) "add scalar to coeffs" (+.) (+.)

    describe "Natural" $ do
      testUnary @Natural @_ @Rust.RustNatural "conversion" id id

instance Arbitrary Natural where
  arbitrary = fromInteger . abs <$> arbitrary

deriving newtype instance Arbitrary Haskell.BLS12_381_GT
