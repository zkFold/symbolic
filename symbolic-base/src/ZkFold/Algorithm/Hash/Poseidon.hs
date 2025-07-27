module ZkFold.Algorithm.Hash.Poseidon (
  module ZkFold.Algorithm.Hash.Poseidon,
  module ZkFold.Algorithm.Hash.Poseidon.Constants,
) where

import Data.List (transpose)
import qualified Data.Vector as V
import Numeric.Natural (Natural)
import Prelude (error, length, map, ($), (.), (==), (<), Maybe(..), reverse, splitAt, (++), Bool(..))
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algorithm.Hash.Poseidon.Constants

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
    dotProduct v1 v2 = V.sum $ V.zipWith (*) v1 v2

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
    let totalRounds = fullRounds params + partialRounds params + fullRounds params
        constantChunks = chunkVector (width params) (roundConstants params)
    in if V.length constantChunks < fromIntegral totalRounds
        then error "Not enough round constants"
        else go initialState constantChunks 0 (fullRounds params) (partialRounds params) (fullRounds params)
  where
    go state constants remainingFirstFull remainingPartial remainingLastFull
      | remainingFirstFull > 0 = 
          let newState = poseidonRound params state (V.head constants) True
          in go newState (V.tail constants) (remainingFirstFull P.- 1) remainingPartial remainingLastFull
      | remainingPartial > 0 = 
          let newState = poseidonRound params state (V.head constants) False
          in go newState (V.tail constants) 0 (remainingPartial P.- 1) remainingLastFull
      | remainingLastFull > 0 = 
          let newState = poseidonRound params state (V.head constants) True
          in go newState (V.tail constants) 0 0 (remainingLastFull P.- 1)
      | otherwise = state

-- | Helper function to chunk a vector into smaller vectors
chunkVector :: Natural -> V.Vector a -> V.Vector (V.Vector a)
chunkVector chunkSize vec =
    let chunks = go (V.toList vec) []
    in V.fromList chunks
  where
    go [] acc = reverse acc
    go xs acc = 
        let (chunk, rest) = splitAt (fromIntegral chunkSize) xs
        in go rest (V.fromList chunk : acc)
    reverse [] = []
    reverse (x:xs) = reverse xs ++ [x]

-- | Sponge construction for arbitrary-length input
poseidonHash :: (Field a) => PoseidonParams a -> [a] -> a
poseidonHash params input = 
    let rate = width params P.- 1  -- capacity is 1
        paddedInput = padInput rate input
        blocks = chunkInput rate paddedInput
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
    
    absorb params [] = V.replicate (fromIntegral (width params)) zero
    absorb params (block:blocks) = 
        let initialState = V.replicate (fromIntegral (width params)) zero
            stateWithBlock = V.zipWith (+) initialState (V.fromList (block ++ [zero]))
            newState = poseidonPermutation params stateWithBlock
        in absorbLoop params newState blocks
    
    absorbLoop _ state [] = state
    absorbLoop params state (block:blocks) = 
        let stateWithBlock = V.zipWith (+) state (V.fromList (block ++ replicate (fromIntegral (width params) P.- length block) zero))
            newState = poseidonPermutation params stateWithBlock
        in absorbLoop params newState blocks
    
    squeeze state = V.head state

-- | Poseidon hash with default parameters
poseidonHashDefault :: (Field a) => [a] -> a
poseidonHashDefault = poseidonHash defaultPoseidonParams