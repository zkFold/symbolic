module Tests.Symbolic.Algorithm.Poseidon where

import qualified Data.Vector as V
import Test.Hspec
import Test.QuickCheck
import Prelude (($))

import ZkFold.Algorithm.Hash.Poseidon
import ZkFold.Symbolic.Algorithm.Hash.Poseidon
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Algebra.EllipticCurve.BN254 (Fr)

tests :: Spec
tests = describe "Poseidon hash tests" $ do
  it "Poseidon hash should produce consistent results" $ do
    -- Test with empty input
    let input1 = [] :: [Fr]
    let result1 = poseidonHashDefault input1
    result1 `shouldNotBe` (0 :: Fr)

  it "Poseidon hash should be deterministic" $ do
    -- Test that same input produces same output
    let input = [1, 2, 3] :: [Fr]
    let result1 = poseidonHashDefault input
    let result2 = poseidonHashDefault input
    result1 `shouldBe` result2

  it "Poseidon hash should handle different inputs differently" $ do
    -- Test that different inputs produce different outputs (with high probability)
    let input1 = [1, 2, 3] :: [Fr]
    let input2 = [3, 2, 1] :: [Fr]
    let result1 = poseidonHashDefault input1
    let result2 = poseidonHashDefault input2
    result1 `shouldNotBe` result2

  it "Poseidon hash should handle single element input" $ do
    -- Test with single element
    let input = [42] :: [Fr]
    let result = poseidonHashDefault input
    result `shouldNotBe` (0 :: Fr)

  it "Poseidon permutation should not be identity" $ do
    -- Test that permutation actually changes the state
    let params = defaultPoseidonParams :: PoseidonParams Fr
    let initialState = V.fromList [1, 2, 3] :: V.Vector Fr
    let result = poseidonPermutation params initialState
    result `shouldNotBe` initialState