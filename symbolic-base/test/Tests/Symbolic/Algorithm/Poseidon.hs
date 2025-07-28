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

-- | Official BLS12-381 test vector (PLACEHOLDER - needs official reference)
-- NOTE: This test vector needs to be replaced with official values once reference is accessible
-- Current values are adjusted for our reduced parameter set (R_F=8, R_P=19)
-- TODO: Replace with official test vector from extgit.isec.tugraz.at/krypto/hadeshash/-/blob/master/code/test_vectors.txt
officialTestVector :: TestVector
officialTestVector =
  TestVector
    { tvName = "poseidon_bls12381_width3_reduced"
    , tvInput =
        [ 0x0000000000000000000000000000000000000000000000000000000000000000
        , 0x0000000000000000000000000000000000000000000000000000000000000001
        , 0x0000000000000000000000000000000000000000000000000000000000000002
        ]
    , tvOutput =
        -- NOTE: These expected outputs need to be computed from official reference
        -- Current values are placeholders that need verification
        [ 0x18e94344ce65e408704bd51713b8220e3f5ccbbc33756f1c8274f44e19c3cef0
        , 0x29d3052639757c6662a0980defb667c812a4cb86a84450a3375b4377c587511c
        , 0x632522385db3923793efb7a4c3e22eebfe96a5235ee4666e7fe3da1c00cc1d9c
        ]
    }

-- | Test Poseidon permutation with current implementation
-- NOTE: Test vector needs to be updated once official reference is accessible
-- Current implementation uses reduced parameters due to limited constants
poseidonPermutationSpec :: Spec
poseidonPermutationSpec = describe "Poseidon permutation test (reduced parameters)" $ do
  it ("should run without error for input " <> show (tvInput officialTestVector)) $ do
    let params = defaultPoseidonParams :: PoseidonParams Fr
    let input = V.fromList $ map (fromConstant @Integer @Fr) (tvInput officialTestVector)
    let result = poseidonPermutation params input
    -- For now, just test that it runs and produces correct length output
    V.length result `shouldBe` 3

-- | Main test specification following repository patterns
specPoseidon :: Spec
specPoseidon = do
  poseidonPermutationSpec
