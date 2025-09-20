module Tests.Data.Binary (specBinary) where

import Data.Binary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Prelude

import ZkFold.Algebra.EllipticCurve.BLS12_381 (
  BLS12_381_G1_CompressedPoint,
  BLS12_381_G1_Point,
  BLS12_381_G2_Point,
  BLS12_381_Scalar,
 )
import ZkFold.Algebra.EllipticCurve.BN254 (BN254_G1_Point, BN254_G2_Point)
import ZkFold.Algebra.EllipticCurve.Pasta (Pallas_Point, Vesta_Point)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Data.Binary (LittleEndian, fromByteString, toByteString)

doesRoundtrip :: (Binary a, Eq a, Show a) => a -> Property
doesRoundtrip x = do
  let xEncoded = toByteString x
  let xDecoded = fromByteString xEncoded
  xDecoded === Just x

specBinary :: Spec
specBinary = describe "Binary instance" $ do
  prop "roundtrips LittleEndian" $ doesRoundtrip @LittleEndian
  prop "roundtrips Zp BLS12_381_Scalar" $ doesRoundtrip @(Zp BLS12_381_Scalar)
  prop "roundtrips Point BN254_G1" $ doesRoundtrip @BN254_G1_Point
  prop "roundtrips Point BN254_G2" $ doesRoundtrip @BN254_G2_Point
  prop "roundtrips Point BLS12_381_G1" $ doesRoundtrip @BLS12_381_G1_Point
  prop "roundtrips CompressedPoint BLS12_381_G1" $ doesRoundtrip @BLS12_381_G1_CompressedPoint
  prop "roundtrips Point BLS12_381_G2" $ doesRoundtrip @BLS12_381_G2_Point
  prop "roundtrips Point Pallas" $ doesRoundtrip @Pallas_Point
  prop "roundtrips Point Vesta" $ doesRoundtrip @Vesta_Point
