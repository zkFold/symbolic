module Tests.Symbolic.Algorithm.Poseidon (specPoseidon) where

import Data.Function (($))
import qualified Data.Vector as V
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude (Integer, String, map, (<>))
import qualified Prelude as Haskell

import ZkFold.Algebra.Class (FromConstant (..))
import ZkFold.Algebra.EllipticCurve.BLS12_381 (Fr)
import ZkFold.Algorithm.Hash.Poseidon

-- | Test vector data type
data TestVector = TestVector
  { tvName :: String
  , tvInput :: [Integer]
  , tvOutput :: [Integer]
  }
  deriving Haskell.Show

-- | Official BLS12-381 test vector from Hades paper reference implementation
-- Source: https://extgit.isec.tugraz.at/krypto/hadeshash/-/blob/master/code/test_vectors.txt
-- Test case: poseidonperm_x5_255_3 - Poseidon permutation for BLS12-381 field with width=3
-- Input: [0, 1, 2] (as field elements)
-- Expected output from reference Sage implementation
officialTestVector :: TestVector
officialTestVector =
  TestVector
    { tvName = "poseidonperm_x5_255_3"
    , tvInput =
        [ 0x0000000000000000000000000000000000000000000000000000000000000000
        , 0x0000000000000000000000000000000000000000000000000000000000000001
        , 0x0000000000000000000000000000000000000000000000000000000000000002
        ]
    , tvOutput =
        [ 0x18e94344ce65e408704bd51713b8220e3f5ccbbc33756f1c8274f44e19c3cef0
        , 0x29d3052639757c6662a0980defb667c812a4cb86a84450a3375b4377c587511c
        , 0x632522385db3923793efb7a4c3e22eebfe96a5235ee4666e7fe3da1c00cc1d9c
        ]
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

-- | Main test specification following repository patterns
specPoseidon :: Spec
specPoseidon = do
  poseidonPermutationSpec
