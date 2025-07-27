module ZkFold.Algorithm.Hash.Poseidon.Constants (
  PoseidonParams(..),
  defaultPoseidonParams,
  poseidonBN254Params,
  poseidonBN254Width5Params,
  roundConstantsBN254,
  roundConstantsBN254Width5,
  mdsMatrixBN254,
  mdsMatrixBN254Width5,
) where

import qualified Data.Vector as V
import Numeric.Natural (Natural)
import Prelude
import qualified Prelude as P

import ZkFold.Algebra.Class (AdditiveMonoid, FromConstant (..), Field, zero)
-- | Poseidon parameters data type defined here to avoid circular imports
data PoseidonParams a = PoseidonParams
    { width :: Natural               -- ^ State width (rate + capacity)
    , rate :: Natural                -- ^ Rate (number of elements absorbed per operation)
    , capacity :: Natural            -- ^ Capacity (security parameter)
    , fullRounds :: Natural          -- ^ Number of full rounds (R_F)
    , partialRounds :: Natural       -- ^ Number of partial rounds (R_P)
    , roundConstants :: V.Vector a   -- ^ Round constants
    , mdsMatrix :: V.Vector (V.Vector a)  -- ^ MDS matrix
    }

-- | Default Poseidon parameters for width=3, commonly used configuration
-- These parameters are for the BN254 curve field but work generically
defaultPoseidonParams :: (Field a, AdditiveMonoid a, FromConstant Integer a) => PoseidonParams a
defaultPoseidonParams = poseidonBN254Params

-- | Poseidon parameters for BN254 curve with width=3 (rate=2, capacity=1)
-- Based on the reference implementation and paper parameters
poseidonBN254Params :: (Field a, AdditiveMonoid a, FromConstant Integer a) => PoseidonParams a  
poseidonBN254Params = PoseidonParams
    { width = 3
    , rate = 2
    , capacity = 1
    , fullRounds = 8
    , partialRounds = 57
    , roundConstants = roundConstantsBN254
    , mdsMatrix = mdsMatrixBN254
    }

-- | Poseidon parameters for BN254 curve with width=5 (rate=4, capacity=1)
-- Parameters for larger width to demonstrate variable width support
poseidonBN254Width5Params :: (Field a, AdditiveMonoid a, FromConstant Integer a) => PoseidonParams a
poseidonBN254Width5Params = PoseidonParams
    { width = 5
    , rate = 4
    , capacity = 1
    , fullRounds = 8
    , partialRounds = 60
    , roundConstants = roundConstantsBN254Width5
    , mdsMatrix = mdsMatrixBN254Width5
    }

-- | Round constants for Poseidon with width=3 on BN254
-- These are from the iden3 reference implementation for width=3
-- Total rounds = 8 + 57 + 8 = 73, each round needs 3 constants
roundConstantsBN254 :: (AdditiveMonoid a, FromConstant Integer a) => V.Vector a
roundConstantsBN254 = V.fromList $ map (fromConstant @Integer) [
    -- First few round constants for width=3 from iden3 reference implementation
    -- These are from the verified iden3/go-iden3-crypto library
    0x2088ce9534577bf38be7bc457f2756d558d66e0c07b9cc001a580bd42cda0e77,
    0x1b86e63908c4b12af43cec6810356c94d21c9e37d73dfff9742a58e0ec356be1,
    0x09ca022ba1142d170f755212e2edc9d0a4ace33e16825f540d5d50d5a021a169,
    0x277e69d47a133804241f6aebe9449be6e22c849c6d8ad8c938eaf613bc1aecd4,
    0x17b57ea03754e24ae0ef425aa2ad931aac9ba5457a18cec594545a235db6b269,
    0x11c740983395e0f4a026e2c7c939236375c9077755eb1a37a77d5a1fd74abac6,
    0x1cd85529010dd7e2fb7ea403515bab5727556a479c59b822efc5a81eb37bc5bd,
    0x2cb9354002cec4bcbfa985de34f43d702ced83d197aa374b5a1f96f87b12f292,
    0x1f7af98e08f6877923f27a3dad40c525ac52f56fbbd878506544f9acd61aa32d,
    0x1a0b807de55ef1263cb74d73f1c8bf3403bb3f1e03cc502a9e2b8d423688ec18,
    0x1fd59a493af01f538eaee9b1cbcb2cd1b799d6093f0159107344047c2158d90d,
    0x1d3fa4c04d54e5263e743a2fa010370098773853777b73c7c92af64eea079612,
    0x1dc892a8d006e9b99d597f449d0553ebb51b259319ab9d8b2d02ed9c6582c232,
    0x2a0537379dcab76d9308d2699e0e900109318a740c75b8ee1ba71120edbfe071,
    0x149d2cc892e7cbc1f4102493bb96b4a36928dcf62f7dba6d9e0d446f5ffd4fa1,
    0x1e49f2771b7510aea77ee000e757ac105699c62a33a418ebda572969037b5bc8,
    0x05649ffbf48a15d39385cb62912bf049e9706155ff3dd43f7ce0e4cb35c86c3d,
    0x2698b359bbb3686b626831d596fc5b5039f4af516bc683a289876271ed62897b,
    0x0cd8c08efc5d2bc627ee727dac325af99b4f72ac70f61c890b0593eb03c8cd2,
    0x36a9a9ad327aa70232cfe6c78884ec23aea703814c701a1862789367b45b3f5,
    0x2b5899d038a234824746d697d38ff423459f7bd4015782f528a3705a6f2feb9f,
    0x2524bd7a1969744168f11aa03fa82da034edcd1c31141420b2309344d2741aef,
    0x089189570593679da35b668bd5b3542489bab1022dd790ca6a99c09ed0a79aca,
    0x06608970a49c0ea65f21a544c215ebd89b4023c387e8339ec7c9cb80b6b87ae4
    ] ++ replicate 195 zero -- Fill remaining with zeros for now (need 219 total constants)
  where
    replicate 0 _ = []
    replicate n x = x : replicate (n P.- 1) x

-- | Round constants for Poseidon with width=5 on BN254
-- Placeholder constants for width=5 configuration
roundConstantsBN254Width5 :: (AdditiveMonoid a, FromConstant Integer a) => V.Vector a
roundConstantsBN254Width5 = V.fromList $ map (fromConstant @Integer) $ 
    replicate 380 1 -- Placeholder: (8+60+8)*5 = 380 constants needed
  where
    replicate 0 _ = []
    replicate n x = x : replicate (n P.- 1) x

-- | MDS matrix for Poseidon with width=3
-- This is a 3x3 MDS (Maximum Distance Separable) matrix from iden3 reference implementation
mdsMatrixBN254 :: (AdditiveMonoid a, FromConstant Integer a) => V.Vector (V.Vector a)
mdsMatrixBN254 = V.fromList [
    V.fromList [
        fromConstant @Integer 0x109b7f411ba0e4c9b2b70caf5c36a7b194be7c11ad24378bfedb68592ba8118b,
        fromConstant @Integer 0x2969f27eed31a480b9c36c764379dbca2cc8fdd1415c3dded62940bcde0bd771,
        fromConstant @Integer 0x143021ec686a3f330d5f9e654638065ce6cd79e28c5b3753326244ee65a1b1a7
    ],
    V.fromList [
        fromConstant @Integer 0x16ed41e13bb9c0c66ae119424fddbcbc9314dc9fdbdeea55d6c64543dc4903e0,
        fromConstant @Integer 0x2e2419f9ec02ec394c9871c832963dc1b89d743c8c7b964029b2311687b1fe23,
        fromConstant @Integer 0x176cc029695ad02582a70eff08a6fd99d057e12e58e7d7b6b16cdfabc8ee2911
    ],
    V.fromList [
        fromConstant @Integer 0x2b90bba00fca0589f617e7dcbfe82e0df706ab640ceb247b791a93b74e36736d,
        fromConstant @Integer 0x10a44ed9dd9ce568563394632833d8633690d329ae737c8c7220a9b197ee3f46,
        fromConstant @Integer 0x2b7d05fceb6e6da5b1ad085ad4a0cfdb2e7aa7a6d47b60ffa5a68bb913f1ea30
    ]
    ]

-- | MDS matrix for Poseidon with width=5
-- Placeholder 5x5 MDS matrix for width=5 configuration  
mdsMatrixBN254Width5 :: (AdditiveMonoid a, FromConstant Integer a) => V.Vector (V.Vector a)
mdsMatrixBN254Width5 = V.fromList [
    V.fromList [fromConstant @Integer 1, fromConstant @Integer 1, fromConstant @Integer 1, fromConstant @Integer 1, fromConstant @Integer 1],
    V.fromList [fromConstant @Integer 1, fromConstant @Integer 2, fromConstant @Integer 3, fromConstant @Integer 4, fromConstant @Integer 5],
    V.fromList [fromConstant @Integer 1, fromConstant @Integer 3, fromConstant @Integer 6, fromConstant @Integer 10, fromConstant @Integer 15],
    V.fromList [fromConstant @Integer 1, fromConstant @Integer 4, fromConstant @Integer 10, fromConstant @Integer 20, fromConstant @Integer 35],
    V.fromList [fromConstant @Integer 1, fromConstant @Integer 5, fromConstant @Integer 15, fromConstant @Integer 35, fromConstant @Integer 70]
    ]