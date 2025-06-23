module Tests.FFI.RustBLS (specRustBLS) where

import           Prelude                                hiding ((*), (+))
import           Test.Hspec
import           Test.Hspec                             (Spec)
import           Test.QuickCheck                        hiding (scale)

import           ZkFold.Algebra.Class
import qualified ZkFold.Algebra.EllipticCurve.BLS12_381 as Haskell
import           ZkFold.Algebra.Polynomial.Univariate   (PolyVec, castPolyVec)
import           ZkFold.FFI.Rust.Conversion
import           ZkFold.FFI.Rust.Poly
import           ZkFold.FFI.Rust.RustBLS
import           GHC.IO                                 (unsafePerformIO)
import qualified ZkFold.FFI.Rust.Types                  as Rust

specRustBLS :: Spec
specRustBLS = do
    specScalar

type Length = 2000

type Length2 = 4000

specScalar :: Spec
specScalar = do
    describe "Rust BLS12-381 specification" $ do
        describe "Scalar" $ do
            it "should be a valid conversion" $ do
                property $ \(a :: Haskell.Fr) -> a == r2h (h2r a)
            it "should be a valid conversion" $ do
                property $ \(a :: Rust.Fr) -> a == h2r (r2h a)

            it "should be correct: add" $ do
                property $ \(a :: Haskell.Fr) (b :: Haskell.Fr) -> (a + b) == r2h ((h2r a) + (h2r b))

            it "should be correct: mul" $ do
                property $ \(a :: Haskell.Fr) (b :: Haskell.Fr) -> (a * b) == r2h ((h2r a) * (h2r b))

        describe "Point" $ do
            it "should be a valid conversion" $ do
                property $ \(a :: Haskell.BLS12_381_G1_Point) -> a == r2h (h2r a)
            it "should be a valid conversion" $ do
                property $ \(a :: Rust.Rust_BLS12_381_G1_Point) -> a == h2r (r2h a)

            it "should be correct: add" $ do
                property $ \(a :: Haskell.BLS12_381_G1_Point) (b :: Haskell.BLS12_381_G1_Point) -> (a + b) == r2h ((h2r a) + (h2r b))

        describe "Scale" $ do
            it "should be correct" $ do
                property $ \(a :: Haskell.Fr) (b :: Haskell.BLS12_381_G1_Point) -> (scale a b) == r2h (scale (h2r a) (h2r b))

        describe "Scalar Polynomial" $ do
            it "should satisfy conversion" $ do
                property $ \(a :: PolyVec Haskell.Fr Length) -> a == r2h (h2r a)

            it "should be correct: add" $ do
                property $ \(a :: PolyVec Haskell.Fr Length) (b :: PolyVec Haskell.Fr Length) -> (a + b) == r2h ((h2r a) + (h2r b))

            it "should be correct: mul" $ do
                let castHaskell = castPolyVec @Haskell.Fr @(PolyVec Haskell.Fr) @Length @Length2
                let castRust = castPolyVec @Rust.Fr @(RustPolyVec Rust.Fr) @Length @Length2
                property $ \(a :: PolyVec Haskell.Fr Length) (b :: PolyVec Haskell.Fr Length) -> 
                    (castHaskell a) * (castHaskell b) ==  (r2h (((castRust $ h2r a) * (castRust $ h2r b))))

