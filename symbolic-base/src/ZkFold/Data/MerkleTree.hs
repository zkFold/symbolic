{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Data.MerkleTree where

import Data.Bits (testBit, xor)
import Data.Bool (Bool (..))
import Data.Eq (Eq (..))
import Data.Foldable (foldl)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Functor.Rep (tabulate)
import Data.Maybe (Maybe (..))
import Data.String (fromString)
import Data.Type.Equality (type (~))
import qualified Data.Vector as V
import Data.Zip (zip)
import GHC.Generics hiding (Rep, UInt, from)
import GHC.TypeNats
import Test.QuickCheck (Arbitrary (..))
import ZkFold.Algebra.Class
import ZkFold.Algebra.Field (Zp, fromZp, toZp)
import ZkFold.Control.Conditional (ifThenElse)
import qualified ZkFold.Data.Eq as ZkFold
import ZkFold.Data.Vector hiding (zip, (.:))
import qualified ZkFold.Data.Vector as V
import qualified ZkFold.Prelude as ZkFold
import ZkFold.Symbolic.Algorithm.Hash.MiMC
import ZkFold.Symbolic.Data.Combinators (Iso (from))
import ZkFold.Symbolic.MonadCircuit (IntegralOf, ResidueField)
import Prelude (Int, Show, error, fromInteger, fromIntegral, pure, ($))
import qualified Prelude as P

-- TODO: ResidueField and related types should properly become a part of our base type hierarchy.
-- Currently, its use here is a bit awkward.

type MerkleTreeSize d = 2 ^ (d - 1)

data MerkleTree (d :: Natural) h = MerkleTree
  { mHash :: h
  , mLeaves :: Vector (MerkleTreeSize d) h
  }
  deriving (Generic, Show)

emptyTree
  :: forall d h
   . ( KnownNat (MerkleTreeSize d)
     , Ring h
     )
  => MerkleTree d h
emptyTree = MerkleTree rootHash leaves
 where
  leaves = pure zero :: Vector (2 ^ (d - 1)) h
  rootHash = computeRoot leaves

-- | Hash to use in the Merkle tree
merkleHash
  :: forall h. Ring h => h -> h -> h
merkleHash = mimcHash2 mimcConstants zero

-- | Hash current value with sibling based on bit direction
hashWithSibling :: Ring h => h -> (Bool, h) -> h
hashWithSibling current (bit, sibling) =
  if bit
    then merkleHash sibling current -- current is right child
    else merkleHash current sibling -- current is left child

-- | Computes the merkle proof for a given index in the merkle tree
merkleProve' :: forall d h. (ResidueField h, KnownNat (d - 1)) => MerkleTree d h -> IntegralOf h -> Vector (d - 1) h
merkleProve' (MerkleTree _ leaves) idx =
  let allLevels = computeAllLevels leaves
   in generateProof allLevels idx
 where
  generateProof :: [[h]] -> IntegralOf h -> Vector (d - 1) h
  generateProof levels startIdx = tabulate $ \proofLevel ->
    let two = one + one
        levelIndex = fromZp proofLevel
        currentIdx = startIdx `div` (two ^ levelIndex)
        siblingIdx = currentIdx + (if mod currentIdx two ZkFold.== zero then one else negate one)
        level = levels ZkFold.!! levelIndex
        f [] _ = error "Merkle tree: impossible"
        f (x : xs) n = if n ZkFold.== zero then x else f xs (n - one)
     in f level siblingIdx

-- | Computes the merkle proof for a given index in the merkle tree
merkleProve :: forall d h. (Ring h, KnownNat (d - 1)) => MerkleTree d h -> Zp (MerkleTreeSize d) -> Vector (d - 1) h
merkleProve (MerkleTree _ leaves) idx =
  let allLevels = computeAllLevels leaves
      indexNat = fromZp idx
   in generateProof allLevels indexNat
 where
  generateProof :: [[h]] -> Natural -> Vector (d - 1) h
  generateProof levels startIdx = tabulate $ \proofLevel ->
    let levelIndex = fromIntegral $ fromZp proofLevel
        currentIdx = startIdx `div` (2 P.^ levelIndex)
        siblingIdx = currentIdx `xor` 1
        level = levels P.!! levelIndex
     in level P.!! fromIntegral siblingIdx

-- | Verifies the merkle proof for a given index in the merkle tree
merkleVerify
  :: forall d h. (Ring h, Eq h, KnownNat (d - 1)) => MerkleTree d h -> Zp (MerkleTreeSize d) -> Vector (d - 1) h -> Bool
merkleVerify (MerkleTree rootHash leaves) idx proof =
  let leaf = leaves V.!! fromZp idx
      indexBits = indexToBits @(d - 1) $ fromZp idx
   in foldl hashWithSibling leaf (zip indexBits proof) == rootHash

instance
  forall d n h
   . ( MerkleTreeSize d ~ n
     , Ring h
     )
  => Iso (Vector n h) (MerkleTree d h)
  where
  from v = MerkleTree (computeRoot v) v

instance
  forall d n h
   . ( MerkleTreeSize d ~ n
     , Ring h
     )
  => Iso (MerkleTree d h) (Vector n h)
  where
  from (MerkleTree _ leaves) = leaves

instance Eq h => Eq (MerkleTree d h) where
  MerkleTree h1 _ == MerkleTree h2 _ = h1 == h2

instance (KnownNat (MerkleTreeSize d), Ring h, Arbitrary h) => Arbitrary (MerkleTree d h) where
  arbitrary = from @(Vector (MerkleTreeSize d) h) <$> arbitrary

-- | Finds an element satisfying the constraint
find
  :: forall d h
   . (h -> Bool)
  -> MerkleTree d h
  -> Maybe h
find p (MerkleTree _ (Vector leaves)) = V.find p leaves

-- | Finds an index of an element satisfying the constraint
findIndex
  :: forall d h
   . KnownNat (MerkleTreeSize d)
  => (h -> Bool)
  -> MerkleTree d h
  -> Maybe (Zp (MerkleTreeSize d))
findIndex p (MerkleTree _ (Vector leaves)) = toZp . fromIntegral <$> V.findIndex p leaves

-- | Returns the index of the first occurrence of an element in the Merkle tree
elemIndex
  :: forall d h
   . (KnownNat (MerkleTreeSize d), Eq h)
  => h
  -> MerkleTree d h
  -> Maybe (Zp (MerkleTreeSize d))
elemIndex h = findIndex (== h)

-- | Returns the element at the given index in the Merkle tree
lookup
  :: forall d h
   . MerkleTree d h
  -> Zp (MerkleTreeSize d)
  -> h
lookup (MerkleTree _ leaves) idx = leaves V.!! fromZp idx

-- | Replaces an element at a given index in the Merkle tree
replaceAt
  :: forall d h
   . Ring h
  => Zp (MerkleTreeSize d)
  -> h
  -> MerkleTree d h
  -> MerkleTree d h
replaceAt idx newLeaf (MerkleTree _ leaves) =
  MerkleTree root' leaves'
 where
  leaves' = Vector $ toV leaves V.// [(fromIntegral $ fromZp idx, newLeaf)]
  root' = computeRoot leaves'

------------------------------- Utilities -------------------------------

-- | Convert index to binary representation (little-endian)
indexToBits :: forall n. KnownNat n => Natural -> Vector n Bool
indexToBits idx =
  let idxInt = fromIntegral idx :: Int
   in tabulate (\i -> testBit idxInt (fromIntegral $ fromZp i))

-- | Computes the next level up in the merkle tree by pairing adjacent elements
computeNextLevel
  :: forall h. Ring h => [h] -> [h]
computeNextLevel [] = []
computeNextLevel [_] = [] -- odd number, ignore the last element
computeNextLevel (a : b : rest) = merkleHash a b : computeNextLevel rest

-- | Computes all levels of the merkle tree from leaves to root
computeAllLevels
  :: forall n h
   . Ring h
  => Vector n h
  -> [[h]]
computeAllLevels leaves = go (fromVector leaves)
 where
  go [] = []
  go [single] = [[single]]
  go current =
    let nextLevel = computeNextLevel current
     in current : go nextLevel

computeRoot
  :: forall d h
   . Ring h
  => Vector (MerkleTreeSize d) h
  -> h
computeRoot leaves =
  case P.last (computeAllLevels leaves) of
    [root] -> root
    _ -> error "Merkle tree: impossible"
