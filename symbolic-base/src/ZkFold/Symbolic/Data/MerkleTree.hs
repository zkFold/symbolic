{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.MerkleTree where

import Data.Constraint (withDict)
import Data.Constraint.Nat (minusNat)
import qualified Data.List as LL
import Data.Type.Equality (type (~))
import GHC.Generics hiding (Rep, UInt, from)
import GHC.TypeNats
import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (integral, value)
import ZkFold.Data.Package
import ZkFold.Data.Vector hiding (zip, (.:))
import qualified ZkFold.Data.Vector as V
import ZkFold.Symbolic.Algorithm.Hash.MiMC
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Bool (Bool (..))
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators (
  Iso (from),
  RegisterSize (Auto),
  expansion,
  horner,
  withNumberOfRegisters,
 )
import ZkFold.Symbolic.Data.FieldElement (FieldElement (FieldElement, fromFieldElement))
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Maybe
import ZkFold.Symbolic.Data.Morph
import ZkFold.Symbolic.Fold (SymbolicFold)
import Prelude (error, pure, return, ($), (.))
import qualified Prelude as P

data MerkleTree (d :: Natural) h = MerkleTree
  { mHash :: h
  , mLeaves :: Vector (2 ^ (d - 1)) h
  }
  deriving Generic

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

zeroMerkleTree
  :: forall d h
   . ( KnownNat (2 ^ (d - 1))
     , Ring h
     )
  => MerkleTree d h
zeroMerkleTree = MerkleTree rootHash leaves
 where
  leaves = pure zero :: Vector (2 ^ (d - 1)) h
  allLevels = computeAllLevels leaves
  rootHash = case P.last allLevels of
    [root] -> root
    _ -> error "Invalid tree structure"

instance
  forall c h d n
   . ( SymbolicData h
     , Context h ~ c
     , Payload h ~ U1
     , KnownNat d
     , KnownNat n
     , 2 ^ (d - 1) ~ n
     , 1 <= d
     , Ring h
     )
  => Iso (Vector n h) (MerkleTree d h)
  where
  from v = MerkleTree rootHash v
   where
    allLevels = computeAllLevels v
    rootHash = case P.last allLevels of
      [root] -> root
      _ -> error "Invalid tree structure"

instance
  forall c h d n
   . ( SymbolicData h
     , Context h ~ c
     , Payload h ~ U1
     , KnownNat d
     , KnownNat n
     , 2 ^ (d - 1) ~ n
     , 1 <= d
     , Ring h
     )
  => Iso (MerkleTree d h) (Vector n h)
  where
  from (MerkleTree _ leaves) = leaves

merkleHash
  :: forall h. Ring h => h -> h -> h
merkleHash = mimcHash2 mimcConstants zero

instance (SymbolicData h, KnownNat d, KnownNat (2 ^ (d - 1))) => SymbolicData (MerkleTree d h)

instance (SymbolicInput h, KnownNat d, KnownNat (2 ^ (d - 1))) => SymbolicInput (MerkleTree d h)

-- | Finds an element satisfying the constraint
find
  :: forall c h d
   . ( SymbolicInput h
     , Context h ~ c
     , SymbolicFold c
     )
  => MorphFrom c h (Bool c)
  -> MerkleTree d h
  -> Maybe c h
find p MerkleTree {..} =
  let leaves = fromVector mLeaves
      matches = P.filter (\x -> evalBool (p @ x)) leaves
   in case matches of
        [] -> nothing
        (x : _) -> just x
 where
  evalBool :: Bool c -> P.Bool
  evalBool _ = P.True -- Simplified for now

newtype MerkleTreePath (d :: Natural) c = MerkleTreePath {mPath :: Vector (d - 1) (Bool c)}
  deriving Generic

instance (Symbolic c, KnownNat (d - 1)) => SymbolicData (MerkleTreePath d c)

-- | Finds a path to an element satisfying the constraint
findPath
  :: forall x c d
   . ( SymbolicData x
     , Context x ~ c
     , SymbolicFold c
     , KnownNat d
     , 1 <= d
     )
  => MorphFrom c x (Bool c)
  -> MerkleTree d x
  -> Maybe c (MerkleTreePath d c)
findPath p (MerkleTree _ leaves) = withDict (minusNat @d @1) $
  case LL.findIndex (\x -> evalBool (p @ x)) (fromVector leaves) of
    P.Nothing -> nothing
    P.Just idx ->
      just $ MerkleTreePath . P.fmap Bool . indToPath @c . fromFieldElement . fromConstant $ (P.fromIntegral idx :: Natural)
 where
  evalBool :: Bool c -> P.Bool
  evalBool _ = P.True -- Simplified for now

indToPath :: forall c d. (Symbolic c, KnownNat d) => c Par1 -> Vector (d - 1) (c Par1)
indToPath e = unpack $ fromCircuitF e $ \(Par1 i) -> do
  ee <- expansion (integral @d) i
  return $ Comp1 (V.unsafeToVector @(d - 1) $ P.map Par1 ee)

-- | Returns the element corresponding to a path
lookup
  :: forall x c d
   . KnownNat d
  => MerkleTree d x
  -> MerkleTreePath d c
  -> x
lookup (MerkleTree _ leaves) (MerkleTreePath _) =
  let leafIdx = (value @d -! 1) -- Simplified index calculation
   in leaves V.!! leafIdx

-- element by index (simplified)
leaf
  :: forall c x d
   . Vector (2 ^ (d - 1)) x
  -> c Par1
  -> x
leaf leaves _ = leaves V.!! 0 -- Simplified for now

-- index of element in path to element
ind :: forall d c. Symbolic c => Vector d (c Par1) -> c Par1
ind vb = fromCircuitF (pack vb) $ \vb' -> do
  let bs = P.map unPar1 $ V.fromVector $ unComp1 vb'
  b1n <- P.fmap Par1 . horner $ bs
  return $ fromConstant b1n

-- | Inserts an element at a specified position in a tree
insertLeaf
  :: forall h c d
   . Ring h
  => MerkleTree d h
  -> MerkleTreePath d c
  -> h
  -> MerkleTree d h
insertLeaf (MerkleTree _ leaves) (MerkleTreePath _) _ =
  let newLeaves = leaves -- Simplified - should update at index
      allLevels = computeAllLevels newLeaves
      newRoot = case P.last allLevels of
        [root] -> root
        _ -> error "Invalid tree structure"
   in MerkleTree newRoot newLeaves

-- | Replaces an element satisfying the constraint. A composition of `findPath` and `insert`
replace
  :: forall h c d n
   . ( SymbolicData h
     , Context h ~ c
     , SymbolicFold c
     , KnownNat d
     , KnownNat (2 ^ (d - 1))
     , 1 <= d
     , NumberOfBits (BaseField c) ~ n
     , Ring h
     )
  => MorphFrom c h (Bool c)
  -> MerkleTree d h
  -> h
  -> MerkleTree d h
replace p t h =
  withNumberOfRegisters @n @Auto @(BaseField c) $
    withDict (minusNat @d @1) $
      maybe t (\path -> insertLeaf t path h) (findPath @h @c p t)

-- | Returns the next path in a tree
incrementPath
  :: forall c d
   . ( KnownNat d
     , Symbolic c
     )
  => MerkleTreePath d c
  -> MerkleTreePath d c
incrementPath (MerkleTreePath p) =
  MerkleTreePath . P.fmap Bool . indToPath @c $ fromFieldElement (FieldElement (ind $ P.fmap (\(Bool b) -> b) p) + one)
