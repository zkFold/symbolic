module Tests.Symbolic.Algorithm.Poseidon (specPoseidon) where

import Data.Function (($))
import qualified Data.Vector as V
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude (Integer, String, map, show, (<>))
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

-- | Official BLS12-381 test vector from poseidonperm_x5_255_3
-- Source: https://extgit.isec.tugraz.at/krypto/hadeshash/-/blob/master/code/test_vectors.txt
-- Field: BLS12-381 scalar field (0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001)
-- Parameters: width=3, R_F=8, R_P=57, S-box=x^5
officialTestVector :: TestVector
officialTestVector =
  TestVector
    { tvName = "poseidon_bls12381_width3_official"
    , tvInput =
        [ 0x0000000000000000000000000000000000000000000000000000000000000000
        , 0x0000000000000000000000000000000000000000000000000000000000000001
        , 0x0000000000000000000000000000000000000000000000000000000000000002
        ]
    , tvOutput =
        -- Official expected output from reference implementation
        [ 0x28ce19420fc246a05553ad1e8c98f5c9d67166be2c18e9e4cb4b4e317dd2a78a
        , 0x51f3e312c95343a896cfd8945ea82ba956c1118ce9b9859b6ea56637b4b1ddc4
        , 0x3b2b69139b235626a0bfb56c9527ae66a7bf486ad8c11c14d1da0c69bbe0f79a
        ]
    }

-- | Test Poseidon permutation against official test vector
poseidonPermutationSpec :: Spec
poseidonPermutationSpec = describe "Poseidon permutation (BLS12-381, width=3, R_F=8, R_P=57)" $ do
  it ("should match official test vector for input " <> show (tvInput officialTestVector)) $ do
    let params = defaultPoseidonParams :: PoseidonParams Fr
    let input = V.fromList $ map (fromConstant @Integer @Fr) (tvInput officialTestVector)
    let expected = V.fromList $ map (fromConstant @Integer @Fr) (tvOutput officialTestVector)
    let result = poseidonPermutation params input
    result `shouldBe` expected

-- | Main test specification following repository patterns
specPoseidon :: Spec
specPoseidon = do
  poseidonPermutationSpec
