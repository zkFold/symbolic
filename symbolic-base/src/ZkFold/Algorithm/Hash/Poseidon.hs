module ZkFold.Algorithm.Hash.Poseidon (
  module ZkFold.Algorithm.Hash.Poseidon,
  module ZkFold.Algorithm.Hash.Poseidon.Constants,
) where

import qualified Data.Vector as V
import Numeric.Natural (Natural)
import Prelude (error, length, map, ($), (<), Maybe(..), splitAt, (++), Bool(..), (>), otherwise, fromIntegral)
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algorithm.Hash.Poseidon.Constants

-- | Poseidon hash function implementation following the Hades design.
-- This implementation follows the official Sage reference implementation from the Hades paper:
-- Paper: "Poseidon: A New Hash Function for Zero-Knowledge Proof Systems" by Grassi et al. (ePrint 2019/458)
-- Reference implementation: https://extgit.isec.tugraz.at/krypto/hadeshash/-/blob/master/code/poseidonperm_x5_255_3.sage
--
-- The Poseidon permutation uses:
-- - S-box: x^5 (α = 5) for efficient implementation in arithmetic circuits
-- - Full rounds: R_F = 8 (4 at beginning + 4 at end)  
-- - Partial rounds: R_P = 57 (middle rounds with S-box applied only to first element)
-- - MDS matrix: 3x3 matrix for optimal diffusion
-- - Field: BN254 scalar field (prime = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001)

-- | Apply S-box (x^α) to a single element
sbox :: (Field a) => a -> a
sbox x = x ^ (5 :: Natural)

-- | Apply S-box to all elements (full round)
fullSboxLayer :: (Field a) => V.Vector a -> V.Vector a
fullSboxLayer = V.map sbox

-- | Apply S-box to first element only (partial round)
partialSboxLayer :: (Field a) => V.Vector a -> V.Vector a
partialSboxLayer state = 
  case V.uncons state of
    Nothing -> state
    Just (first, rest) -> V.cons (sbox first) rest

-- | Add round constants to state
addRoundConstants :: (AdditiveGroup a) => V.Vector a -> V.Vector a -> V.Vector a
addRoundConstants state constants = V.zipWith (+) state constants

-- | Matrix multiplication for MDS layer
mdsLayer :: (Ring a) => V.Vector (V.Vector a) -> V.Vector a -> V.Vector a
mdsLayer matrix state = V.fromList $ map (dotProduct state) (V.toList matrix)
  where
    dotProduct v1 v2 = V.foldr (+) zero $ V.zipWith (*) v1 v2

-- | Single Poseidon round
poseidonRound :: (Field a) => 
    PoseidonParams a -> 
    V.Vector a ->     -- ^ current state
    V.Vector a ->     -- ^ round constants for this round
    Bool ->           -- ^ is full round
    V.Vector a        -- ^ new state
poseidonRound params state constants isFull =
    let stateWithConstants = addRoundConstants state constants
        stateAfterSbox = if isFull 
            then fullSboxLayer stateWithConstants
            else partialSboxLayer stateWithConstants
    in mdsLayer (mdsMatrix params) stateAfterSbox

-- | Poseidon permutation
poseidonPermutation :: (Field a) => PoseidonParams a -> V.Vector a -> V.Vector a
poseidonPermutation params initialState = 
    let totalConstantsNeeded = (fullRounds params * 2 + partialRounds params) * width params
        allConstants = roundConstants params
    in if V.length allConstants == 0
        then error "No round constants available"
        else go initialState allConstants (fullRounds params) (partialRounds params) (fullRounds params) 0
  where    
    go state constants remainingFirstFull remainingPartial remainingLastFull constantIndex
      | remainingFirstFull > 0 = 
          let roundConstants = getNextConstants (fromIntegral (width params)) constantIndex
              newState = poseidonRound params state roundConstants True
              nextIndex = constantIndex + fromIntegral (width params)
          in go newState constants (remainingFirstFull P.- 1) remainingPartial remainingLastFull nextIndex
      | remainingPartial > 0 = 
          let roundConstants = getNextConstants (fromIntegral (width params)) constantIndex  
              newState = poseidonRound params state roundConstants False
              nextIndex = constantIndex + fromIntegral (width params)
          in go newState constants 0 (remainingPartial P.- 1) remainingLastFull nextIndex
      | remainingLastFull > 0 = 
          let roundConstants = getNextConstants (fromIntegral (width params)) constantIndex
              newState = poseidonRound params state roundConstants True
              nextIndex = constantIndex + fromIntegral (width params)
          in go newState constants 0 0 (remainingLastFull P.- 1) nextIndex
      | otherwise = state
    
    getNextConstants count startIndex = 
        let availableConstants = V.length (roundConstants params)
        in V.generate count $ \i -> 
            let idx = (startIndex + i) `mod` availableConstants
            in (roundConstants params) V.! idx

-- | Helper function to chunk a vector into smaller vectors
chunkVector :: Natural -> V.Vector a -> V.Vector (V.Vector a)
chunkVector chunkSize vec =
    let chunks = go (V.toList vec) []
    in V.fromList chunks
  where
    go [] acc = reverseList acc
    go xs acc = 
        let (chunk, rest) = splitAt (fromIntegral chunkSize) xs
        in go rest (V.fromList chunk : acc)
    reverseList [] = []
    reverseList (x:xs) = reverseList xs ++ [x]

-- | Sponge construction for arbitrary-length input
poseidonHash :: (Field a) => PoseidonParams a -> [a] -> a
poseidonHash params input = 
    let r = rate params
        paddedInput = padInput r input
        blocks = chunkInput r paddedInput
        finalState = absorb params blocks
    in squeeze finalState
  where
    padInput r inp = 
        let inputLen = fromIntegral (length inp)
            paddingLen = r P.- (inputLen `P.mod` r)
        in inp ++ replicate (fromIntegral paddingLen) zero
    
    chunkInput r inp = chunkList (fromIntegral r) inp
    
    chunkList _ [] = []
    chunkList n xs = 
        let (chunk, rest) = splitAt n xs
        in chunk : chunkList n rest
    
    replicate 0 _ = []
    replicate n x = x : replicate (n P.- 1) x
    
    absorb prms [] = V.replicate (fromIntegral (width prms)) zero
    absorb prms (block:blocks) = 
        let initialState = V.replicate (fromIntegral (width prms)) zero
            stateWithBlock = V.zipWith (+) initialState (V.fromList (block ++ replicate (fromIntegral (capacity prms)) zero))
            newState = poseidonPermutation prms stateWithBlock
        in absorbLoop prms newState blocks
    
    absorbLoop _ state [] = state
    absorbLoop prms state (block:blocks) = 
        let stateWithBlock = V.zipWith (+) state (V.fromList (block ++ replicate (fromIntegral (capacity prms)) zero))
            newState = poseidonPermutation prms stateWithBlock
        in absorbLoop prms newState blocks
    
    squeeze state = V.head state

-- | Poseidon hash with default parameters
poseidonHashDefault :: (Field a) => [a] -> a
poseidonHashDefault = poseidonHash defaultPoseidonParams