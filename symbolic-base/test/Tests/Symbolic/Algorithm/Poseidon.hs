module Tests.Symbolic.Algorithm.Poseidon (tests) where

import Control.Monad (forM_)
import Data.Function (($))
import Data.List (take)
import Data.List.Split (splitOn)
import System.IO (IO)
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldNotBe)
import Text.Read (read)
import Prelude (Integer, String, map, (<>))
import qualified Data.Vector as V
import qualified Prelude as Haskell

import ZkFold.Algebra.Class (FromConstant(..), zero)
import ZkFold.Algorithm.Hash.Poseidon
import ZkFold.Symbolic.Algorithm.Hash.Poseidon
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Algebra.EllipticCurve.BN254 (Fr)

-- | Test against known behavior patterns following repository conventions
behaviorTests :: Spec
behaviorTests = describe "Poseidon hash behavioral validation" $ do
  it "should be deterministic - same input produces same output" $ do
    let input = [1, 2] :: [Fr]
    let result1 = poseidonHashDefault input
    let result2 = poseidonHashDefault input
    result1 `shouldBe` result2

  it "should produce different outputs for different inputs" $ do
    let input1 = [1, 2] :: [Fr]
    let input2 = [2, 1] :: [Fr]
    let result1 = poseidonHashDefault input1
    let result2 = poseidonHashDefault input2
    result1 `shouldNotBe` result2

  it "should produce different outputs for reversed inputs" $ do
    let input1 = [1, 2] :: [Fr]
    let input2 = [2, 1] :: [Fr]
    let result1 = poseidonHashDefault input1
    let result2 = poseidonHashDefault input2
    result1 `shouldNotBe` result2

  it "should handle zero inputs correctly" $ do
    let input = [0, 0] :: [Fr]
    let result = poseidonHashDefault input
    -- Zero input should still produce a non-zero hash due to round constants
    result `shouldNotBe` (zero :: Fr)

  it "should handle single non-zero with zero padding" $ do
    let input = [42, 0] :: [Fr]
    let result = poseidonHashDefault input
    result `shouldNotBe` (zero :: Fr)

  it "should produce different results for different positions of same value" $ do
    let input1 = [42, 0] :: [Fr]
    let input2 = [0, 42] :: [Fr]
    let result1 = poseidonHashDefault input1
    let result2 = poseidonHashDefault input2
    result1 `shouldNotBe` result2

-- | Test that permutation is not identity
permutationTests :: Spec
permutationTests = describe "Poseidon permutation correctness" $ do
  it "permutation should not be identity function" $ do
    let params = defaultPoseidonParams :: PoseidonParams Fr
    let initialState = V.fromList [1, 2, 3] :: V.Vector Fr
    let result = poseidonPermutation params initialState
    result `shouldNotBe` initialState

  it "permutation should be deterministic" $ do
    let params = defaultPoseidonParams :: PoseidonParams Fr
    let initialState = V.fromList [1, 2, 3] :: V.Vector Fr
    let result1 = poseidonPermutation params initialState
    let result2 = poseidonPermutation params initialState
    result1 `shouldBe` result2

  it "different initial states should give different results" $ do
    let params = defaultPoseidonParams :: PoseidonParams Fr
    let state1 = V.fromList [1, 2, 3] :: V.Vector Fr
    let state2 = V.fromList [3, 2, 1] :: V.Vector Fr
    let result1 = poseidonPermutation params state1
    let result2 = poseidonPermutation params state2
    result1 `shouldNotBe` result2

-- | Test edge cases and robustness
edgeCaseTests :: Spec
edgeCaseTests = describe "Poseidon hash edge cases" $ do
  it "should handle empty input (padded to width)" $ do
    let input = [] :: [Fr]
    let result = poseidonHashDefault input
    result `shouldNotBe` (zero :: Fr)

  it "should handle single element input" $ do
    let input = [1] :: [Fr]
    let result = poseidonHashDefault input
    result `shouldNotBe` (zero :: Fr)

  it "should handle longer input sequences" $ do
    let input = [1, 2, 3, 4, 5] :: [Fr]
    let result = poseidonHashDefault input
    result `shouldNotBe` (zero :: Fr)

-- | Main test specification following repository patterns
tests :: Spec
tests = do
  behaviorTests
  permutationTests  
  edgeCaseTests