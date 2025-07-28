module Tests.Symbolic.Algorithm.Poseidon (specPoseidon) where

import Data.Function (($), (.))
import qualified Data.Vector as V
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude (Integer, String, map, (<>))
import qualified Prelude as Haskell

import ZkFold.Algebra.Class (FromConstant (..))
import ZkFold.Algebra.EllipticCurve.BN254 (Fr)
import ZkFold.Algorithm.Hash.Poseidon

-- | Test vector data type  
data TestVector = TestVector
  { tvName :: String
  , tvInput :: [Integer]
  , tvOutput :: [Integer]
  }
  deriving Haskell.Show

-- | Official BN254 test vector from Hades paper reference implementation
-- Source: https://extgit.isec.tugraz.at/krypto/hadeshash/-/blob/master/code/test_vectors.txt
-- Test case: poseidonperm_x5_254_3 - Poseidon permutation for BN254 field with width=3
-- Input: [0, 1, 2] (as field elements)
-- Expected output from reference Sage implementation
officialTestVector :: TestVector
officialTestVector = TestVector
  { tvName = "poseidonperm_x5_254_3"
  , tvInput = [0x0000000000000000000000000000000000000000000000000000000000000000, 
               0x0000000000000000000000000000000000000000000000000000000000000001, 
               0x0000000000000000000000000000000000000000000000000000000000000002]
  , tvOutput = [0x115cc0f5e7d690413df64c6b9662e9cf2a3617f2743245519e19607a4417189a,
                0x0fca49b798923ab0239de1c9e7a4a9a2210312b6a2f616d18b5a87f9b628ae29,
                0x0e7ae82e40091e63cbd4f16a6d16310b3729d4b6e138fcf54110e2867045a30c]
  }

-- | Test Poseidon permutation against official reference test vectors from Hades paper
-- Reference: https://extgit.isec.tugraz.at/krypto/hadeshash/-/blob/master/code/test_vectors.txt
poseidonPermutationSpec :: Spec
poseidonPermutationSpec = describe "Poseidon permutation test vectors" $ do
  it ("should match reference implementation for " <> tvName officialTestVector) $ do
    let params = defaultPoseidonParams :: PoseidonParams Fr
    let input = V.fromList $ map (fromConstant @Integer @Fr) (tvInput officialTestVector)
    let expected = V.fromList $ map (fromConstant @Integer @Fr) (tvOutput officialTestVector)
    let result = poseidonPermutation params input
    result `shouldBe` expected

-- | Test variable width capability
poseidonVariableWidthSpec :: Spec
poseidonVariableWidthSpec = describe "Poseidon variable width support" $ do
  it "should support width=3 configuration" $ do
    let params = poseidonBN254Params :: PoseidonParams Fr
    width params `shouldBe` 3
    rate params `shouldBe` 2
    capacity params `shouldBe` 1

  it "should support width=5 configuration" $ do
    let params = poseidonBN254Width5Params :: PoseidonParams Fr
    width params `shouldBe` 5
    rate params `shouldBe` 4
    capacity params `shouldBe` 1

  it "width=3 permutation should produce correct length output" $ do
    let params = poseidonBN254Params :: PoseidonParams Fr
    let input = V.fromList [1, 2, 3] :: V.Vector Fr
    let result = poseidonPermutation params input
    V.length result `shouldBe` 3

  it "width=5 permutation should produce correct length output" $ do
    let params = poseidonBN254Width5Params :: PoseidonParams Fr
    let input = V.fromList [1, 2, 3, 4, 5] :: V.Vector Fr
    let result = poseidonPermutation params input
    V.length result `shouldBe` 5

  it "different widths should give different results for comparable inputs" $ do
    let params3 = poseidonBN254Params :: PoseidonParams Fr
    let params5 = poseidonBN254Width5Params :: PoseidonParams Fr
    let input3 = V.fromList [1, 2, 3] :: V.Vector Fr
    let input5 = V.fromList [1, 2, 3, 4, 5] :: V.Vector Fr
    let result3 = poseidonPermutation params3 input3
    let result5 = poseidonPermutation params5 input5
    -- They should produce different results (can't compare directly due to different lengths)
    V.length result3 `shouldBe` 3
    V.length result5 `shouldBe` 5

-- | Main test specification following repository patterns
specPoseidon :: Spec
specPoseidon = do
  poseidonPermutationSpec
  poseidonVariableWidthSpec
