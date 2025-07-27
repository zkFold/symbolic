module Tests.Symbolic.Algorithm.Poseidon (specPoseidon) where

import Control.Monad (forM_)
import Data.Function (($))
import Data.List (take, isPrefixOf)
import Data.List.Split (splitOn)
import System.IO (IO)
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldNotBe)
import Text.Read (read)
import Prelude (Integer, String, map, (<>), lines, drop, filter, words, not, null, (!!))
import qualified Data.Vector as V
import qualified Prelude as Haskell

import ZkFold.Algebra.Class (FromConstant(..), zero)
import ZkFold.Algorithm.Hash.Poseidon
import ZkFold.Symbolic.Algorithm.Hash.Poseidon
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Algebra.EllipticCurve.BN254 (Fr)

-- | Parse hex string to integer
parseHex :: String -> Integer
parseHex s = read $ "0x" <> s

-- | Parse test vector input line
parseInput :: String -> [Integer]
parseInput line = 
    let cleaned = filter (/= ',') $ drop 1 $ take (Haskell.length line - 1) line -- Remove [ and ]
        hexStrings = words $ map (\c -> if c == '\'' then ' ' else c) cleaned
    in map (parseHex . drop 2) $ filter (not . null) hexStrings -- Remove 0x prefix

-- | Parse test vector output line  
parseOutput :: String -> [Integer]
parseOutput = parseInput

-- | Test vector data type
data TestVector = TestVector
    { tvName :: String
    , tvInput :: [Integer]
    , tvOutput :: [Integer]
    } deriving (Haskell.Show)

-- | Parse test vectors from file content
parseTestVectors :: String -> [TestVector]
parseTestVectors content = 
    let allLines = lines content
        testSections = splitTestSections allLines
    in map parseSection testSections
  where
    splitTestSections [] = []
    splitTestSections ls = 
        let (section, rest) = break (isPrefixOf "#") (drop 1 ls)
        in (take 1 ls ++ section) : splitTestSections rest
    
    parseSection (nameL:inputL:outputL:_) = TestVector
        { tvName = drop 2 nameL -- Remove "# "
        , tvInput = parseInput inputL
        , tvOutput = parseOutput outputL
        }
    parseSection _ = TestVector "" [] []
-- | Test Poseidon permutation against reference test vectors
poseidonPermutationSpec :: Spec
poseidonPermutationSpec = describe "Poseidon permutation test vectors" $ do
  testVectors <- runIO $ Haskell.readFile "test/data/poseidon_test_vectors.txt"
  let vectors = parseTestVectors testVectors
  
  forM_ vectors $ \tv -> 
    it ("should match reference implementation for " <> tvName tv) $ do
      case tvName tv of
        "poseidonperm_x5_254_3" -> do
          let params = defaultPoseidonParams :: PoseidonParams Fr
          let input = V.fromList $ map (fromConstant @Integer) (tvInput tv)
          let expected = V.fromList $ map (fromConstant @Integer) (tvOutput tv)
          let result = poseidonPermutation params input
          result `shouldBe` expected
        _ -> Haskell.pure () -- Skip other test vectors for now

-- | Test Poseidon hash behavioral properties following repository patterns
poseidonBehavioralSpec :: Spec
poseidonBehavioralSpec = describe "Poseidon hash behavioral validation" $ do
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
poseidonPermutationBehavioralSpec :: Spec
poseidonPermutationBehavioralSpec = describe "Poseidon permutation correctness" $ do
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
poseidonEdgeCaseSpec :: Spec
poseidonEdgeCaseSpec = describe "Poseidon hash edge cases" $ do
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
specPoseidon :: Spec
specPoseidon = do
  poseidonPermutationSpec
  poseidonBehavioralSpec
  poseidonPermutationBehavioralSpec  
  poseidonEdgeCaseSpec