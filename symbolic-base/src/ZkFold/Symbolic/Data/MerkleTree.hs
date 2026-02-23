{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.MerkleTree (
  MerkleTree (mHash),
  emptyTree,
  fromLeaves,
  toLeaves,
  MerklePath,
  merklePath,
  rootOnReplace,
  MerkleEntry (..),
  contains,
  (!!),
  search,
  find,
  findIndex,
  elemIndex,
  lookup,
  search',
  KnownMerkleTree,
  replace,
  replaceAt,
  Index,
) where

import Data.Bool (otherwise)
import Data.Foldable (foldl', foldr, toList)
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.List (splitAt, length)
import Data.Ord ((<=))
import Data.Tuple (fst)
import Data.Type.Equality (type (~))
import qualified Data.Vector as Data
import Data.Zip (zipWith)
import GHC.Generics (Generic, Generic1, Par1 (Par1, unPar1), U1 (..), (:*:) (..), (:.:) (..))
import GHC.TypeLits (KnownNat, type (-), type (^))
import Test.QuickCheck (Arbitrary (..))
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algorithm.Hash.MiMC.Constants (mimcConstants)
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq (BooleanOf, Eq, (==))
import ZkFold.Data.HFunctor.Classes (HEq, HShow)
import qualified ZkFold.Data.MerkleTree as Base
import ZkFold.Data.Package (packed)
import qualified ZkFold.Symbolic.Algorithm.Hash.MiMC as SymbolicMiMC
import ZkFold.Data.Product (toPair)
import ZkFold.Data.Vector (Vector, mapWithIx, reverse, toV, unsafeToVector)
import ZkFold.Symbolic.Class (Arithmetic, BaseField, Symbolic, WitnessField, embedW, witnessF)
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (..), Conditional, assert, bool, (||))
import ZkFold.Symbolic.Data.Class (SymbolicData, withoutConstraints)
import ZkFold.Symbolic.Data.FieldElement (FieldElement (FieldElement), fieldElements, fromFieldElement)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Maybe (Maybe, fromJust, guard, mmap)
import ZkFold.Symbolic.Data.Payloaded (Payloaded (..), payloaded, restored)
import ZkFold.Symbolic.Data.Vec (Vec (..))
import ZkFold.Symbolic.Interpreter (Interpreter (runInterpreter))
import ZkFold.Symbolic.WitnessContext (WitnessContext (..))

data MerkleTree d c = MerkleTree
  { mHash :: FieldElement c
  , mLeaves :: Payloaded (Base.Leaves d) FieldElement c
  }
  deriving (Eq, Generic, Generic1, SymbolicData, SymbolicInput)

instance HEq c => P.Eq (MerkleTree d c) where
  MerkleTree rh _ == MerkleTree rh' _ = rh P.== rh'

deriving instance (HShow c, P.Show (WitnessField c)) => P.Show (MerkleTree d c)

emptyTree :: (KnownNat (Base.MerkleTreeSize d), Symbolic c) => MerkleTree d c
emptyTree = fromLeaves zero

fromLeaves :: Symbolic c => Base.Leaves d (FieldElement c) -> MerkleTree d c
fromLeaves src@(payloaded -> mLeaves) = MerkleTree {..}
 where
  mHash = Base.computeRoot src

toLeaves :: Symbolic c => MerkleTree d c -> Vec (Base.Leaves d) c
toLeaves src@MerkleTree {..} =
  assert ((== src) . fromLeaves . fieldElements . runVec) $
    Vec $
      packed $
        fromFieldElement <$> restored mLeaves

instance
  (Symbolic c, BaseField c ~ a, Base.MerkleTreeSize d ~ n)
  => FromConstant (Vector n a) (MerkleTree d c)
  where
  fromConstant = fromLeaves . fmap fromConstant

instance Arithmetic a => ToConstant (MerkleTree d (Interpreter a)) where
  type Const (MerkleTree d (Interpreter a)) = Base.Leaves d a
  toConstant = runInterpreter . runVec . toLeaves

instance
  (Symbolic c, Arbitrary (BaseField c), KnownNat (Base.MerkleTreeSize d))
  => Arbitrary (MerkleTree d c)
  where
  arbitrary = fromLeaves <$> arbitrary

type MerklePath d = Vector (d - 1) :.: (Bool :*: FieldElement)

type Index d = Vector (d - 1) :.: Bool

-- | Compute the merkle path for a given position in the tree.
-- This implementation uses circuit-level operations throughout to avoid
-- the exponential WitnessF closure blowup that occurs with witness-level MiMC.
merklePath
  :: (Symbolic c)
  => MerkleTree d c
  -> Index d c
  -> MerklePath d c
merklePath MerkleTree {..} position =
  let leaves = toList $ restored mLeaves
      levels = computeAllLevelsSymbolic leaves
      bitsReversed = toList $ reverse $ unComp1 position  -- from leaf toward root
      -- Compute siblings and drop constraints - rootOnReplace will verify
      siblings = withoutConstraints <$> computeSiblingsSymbolic levels bitsReversed
   in Comp1 $ unsafeToVector $ P.zipWith (:*:) bitsReversed siblings

-- | Compute all tree levels at the circuit level using the circuit-optimized MiMC.
-- levels[0] = leaves, levels[k] has 2^(n-k) elements, levels[n] = [root]
computeAllLevelsSymbolic :: Symbolic c => [FieldElement c] -> [[FieldElement c]]
computeAllLevelsSymbolic [] = []
computeAllLevelsSymbolic [single] = [[single]]
computeAllLevelsSymbolic current = current : computeAllLevelsSymbolic (computeNextLevelSymbolic current)

-- | Compute the next level of the tree by hashing pairs.
computeNextLevelSymbolic :: Symbolic c => [FieldElement c] -> [FieldElement c]
computeNextLevelSymbolic [] = []
computeNextLevelSymbolic [_] = []
computeNextLevelSymbolic (a : b : rest) = merkleHash a b : computeNextLevelSymbolic rest

-- | Compute siblings at each level based on symbolic index bits.
-- levels: from leaves (level 0) toward root
-- bitsLSB: index bits in LSB-first order (bitsLSB[k] = bit k of index)
-- Returns: sibling values at each level, from level 0 upward
--
-- At level k, the element index is (originalIndex >> k), and the sibling
-- index is that value with its LSB flipped. We select using MSB-first ordering.
computeSiblingsSymbolic :: Symbolic c => [[FieldElement c]] -> [Bool c] -> [FieldElement c]
computeSiblingsSymbolic levels bitsLSB =
  [ selectSiblingAtLevel (levels P.!! k) k
  | k <- [0 .. length bitsLSB P.- 1]
  ]
  where
    selectSiblingAtLevel level k =
      let -- Bits for (index >> k) in LSB-first order
          relevantBitsLSB = P.drop k bitsLSB
          -- Flip the LSB to get sibling index
          siblingBitsLSB = case relevantBitsLSB of
            (b : rest) -> notB b : rest
            [] -> []
          -- Reverse to get MSB-first for selection
          siblingBitsMSB = P.reverse siblingBitsLSB
       in selectFromLevel level siblingBitsMSB
    symTrue = Bool $ embedW $ Par1 one
    symFalse = Bool $ embedW $ Par1 zero

    selectFromLevel [x] _ = x
    selectFromLevel xs (b : bs) =
      let (left, right) = splitAt (length xs `P.div` 2) xs
       in bool (selectFromLevel left bs) (selectFromLevel right bs) b
    selectFromLevel xs [] = P.head xs

    -- Symbolic NOT: if b then False else True
    notB = bool symTrue symFalse

-- | Circuit-optimized MiMC hash for Merkle tree operations.
-- Uses the Symbolic MiMC implementation which builds circuits efficiently
-- rather than creating exponential WitnessF closures.
merkleHash :: Symbolic c => FieldElement c -> FieldElement c -> FieldElement c
merkleHash = SymbolicMiMC.mimcHash2 mimcConstants zero

-- | Hash current value with sibling based on bit direction.
-- Uses the circuit-optimized hash function.
-- Computes inputs conditionally to avoid doubling hash constraints.
hashWithSibling :: Symbolic c => FieldElement c -> (Bool c, FieldElement c) -> FieldElement c
hashWithSibling current (bit, sibling) =
  -- Compute which value goes left/right using cheap conditionals,
  -- then do a single hash instead of computing both orderings.
  let left = bool current sibling bit
      right = bool sibling current bit
  in merkleHash left right

rootOnReplace :: Symbolic c => MerklePath d c -> FieldElement c -> FieldElement c
rootOnReplace (Comp1 path) value =
  foldl' ((. toPair) . hashWithSibling) value path

data MerkleEntry d c = MerkleEntry
  { position :: Index d c
  , value :: FieldElement c
  }
  deriving (Generic, Generic1, SymbolicData, SymbolicInput)

deriving stock instance HShow c => P.Show (MerkleEntry d c)

contains
  :: forall d c
   . (Symbolic c)
  => MerkleTree d c
  -> MerkleEntry d c
  -> Bool c
tree `contains` MerkleEntry {..} =
  rootOnReplace (merklePath tree position) value == mHash tree

type Bool' c = BooleanOf (IntegralOf (WitnessField c))

(!!)
  :: forall d c
   . (Symbolic c)
  => MerkleTree d c
  -> Index d c
  -> FieldElement c
tree !! position =
  assert (\value -> tree `contains` MerkleEntry {..}) $
    fromBaseHash $
      recIndex (fromBool <$> unComp1 position) $
        toBaseLeaves (mLeaves tree)
 where
  recIndex
    :: forall n b a. Conditional b a => Vector n b -> Vector (2 ^ n) a -> a
  recIndex i v = foldr splitter Data.head (toList i) (toV v)
   where
    splitter :: b -> (Data.Vector a -> a) -> Data.Vector a -> a
    splitter b rec d = let (l, r) = bisect d in ifThenElse b (rec r) (rec l)

  fromBool :: Bool c -> Bool' c
  fromBool (Bool b) = (== one) $ toIntegral $ unPar1 $ witnessF b

search
  :: forall c d
   . (Symbolic c)
  => (FieldElement (WitnessContext c) -> Bool (WitnessContext c))
  -> MerkleTree d c
  -> Maybe (MerkleEntry d) c
search pred tree =
  assert (\entry -> tree `contains` fromJust entry) $
    toEntry $
      recSearch
        (fromBool . pred . FieldElement . WC . Par1)
        (toBaseLeaves $ mLeaves tree)
 where
  recSearch
    :: forall n b a
     . (BoolType b, Conditional b a)
    => (a -> b)
    -> Vector (2 ^ n) a
    -> (b, Vector n b, a)
  recSearch p d =
    let (b, i, x) = doSearch (toV d)
     in (b, unsafeToVector i, x)
   where
    doSearch :: Data.Vector a -> (b, [b], a)
    doSearch v
      | Data.length v <= 1 = let x = Data.head v in (p x, [], x)
      | otherwise =
          let (l, r) = bisect v
              (isL, li, lx) = doSearch l
              (isR, ri, rx) = doSearch r
           in ( isL || isR
              , isR : zipWith (ifThenElse isR) ri li
              , ifThenElse isR rx lx
              )

  toEntry :: Bool' c ~ b => (b, Vector (d - 1) b, WitnessField c) -> Maybe (MerkleEntry d) c
  toEntry
    ( toBool -> wasFound
      , Comp1 . fmap toBool -> position
      , fromBaseHash -> value
      ) = guard wasFound MerkleEntry {..}

  fromBool :: Bool (WitnessContext c) -> Bool' c
  fromBool (Bool (WC (Par1 b))) = toIntegral b == one

  toBool :: Bool' c -> Bool c
  toBool = Bool . embedW . Par1 . bool zero one

find
  :: forall c d
   . (Symbolic c)
  => (FieldElement (WitnessContext c) -> Bool (WitnessContext c))
  -> MerkleTree d c
  -> Maybe FieldElement c
find pred = mmap value . search pred

findIndex
  :: forall c d
   . (Symbolic c)
  => (FieldElement (WitnessContext c) -> Bool (WitnessContext c))
  -> MerkleTree d c
  -> Maybe (Index d) c
findIndex pred = mmap position . search pred

elemIndex
  :: forall c d
   . (Symbolic c)
  => FieldElement (WitnessContext c)
  -> MerkleTree d c
  -> Maybe (Index d) c
elemIndex elem = findIndex (== elem)

lookup
  :: (Symbolic c)
  => MerkleTree d c
  -> Index d c
  -> FieldElement c
lookup = (!!)

search'
  :: (Symbolic c)
  => (forall e. (Symbolic e, BaseField e ~ BaseField c) => FieldElement e -> Bool e)
  -> MerkleTree d c
  -> MerkleEntry d c
search' p = assert (p . value) . fromJust . search p

type KnownMerkleTree d = (KnownNat (d - 1), KnownNat (Base.MerkleTreeSize d))

replace
  :: forall c d
   . (Symbolic c, KnownMerkleTree d)
  => MerkleEntry d c
  -> MerkleTree d c
  -> MerkleTree d c
replace MerkleEntry {..} tree =
  -- Verify input tree is consistent along this path before replacement
  assert (\_ -> oldRootValid) result
 where
  path = merklePath tree position

  -- Get old value at position (witness-level selection, no constraints)
  oldValue = fromBaseHash $
    recIndex (fromBool <$> unComp1 position) $
      toBaseLeaves (mLeaves tree)

  -- Verify the path siblings are consistent with the stored root
  oldRootValid = rootOnReplace path oldValue == mHash tree

  -- Compute new state
  newRoot = rootOnReplace path value
  newLeaves =
    let oldLeaves = toBaseLeaves (mLeaves tree)
        updatedLeaves = mapWithIx (replacer (toBasePosition position, toBaseHash value)) oldLeaves
     in Payloaded $ fmap (\v -> (Par1 v, U1)) updatedLeaves
  result = MerkleTree newRoot newLeaves

  recIndex :: forall n b a. Conditional b a => Vector n b -> Vector (2 ^ n) a -> a
  recIndex i v = foldr splitter Data.head (toList i) (toV v)
   where
    splitter b rec d = let (l, r) = bisect d in ifThenElse b (rec r) (rec l)

  fromBool :: Bool c -> Bool' c
  fromBool (Bool b) = (== one) $ toIntegral $ unPar1 $ witnessF b

  replacer
    :: (FromConstant n i, Eq i, Conditional (BooleanOf i) a)
    => (i, a)
    -> n
    -> a
    -> a
  replacer (idx, newVal) n = ifThenElse (idx == fromConstant n) newVal

replaceAt
  :: (Symbolic c, KnownMerkleTree d)
  => Index d c
  -> FieldElement c
  -> MerkleTree d c
  -> MerkleTree d c
replaceAt position value = replace MerkleEntry {..}

---------------------------- conversion functions ------------------------------

bisect :: Data.Vector a -> (Data.Vector a, Data.Vector a)
bisect v = Data.splitAt (Data.length v `P.div` 2) v

fromBaseHash :: Symbolic c => WitnessField c -> FieldElement c
fromBaseHash = FieldElement . embedW . Par1

toBaseHash :: Symbolic c => FieldElement c -> WitnessField c
toBaseHash = unPar1 . witnessF . fromFieldElement

toBaseLeaves
  :: Payloaded (Base.Leaves d) FieldElement c -> Base.Leaves d (WitnessField c)
toBaseLeaves = fmap (unPar1 . fst) . runPayloaded

toBasePosition
  :: forall c d. Symbolic c => Index d c -> IntegralOf (WitnessField c)
toBasePosition = foldl' (\x b -> double x + fromBool b) zero . unComp1
 where
  fromBool :: Bool c -> IntegralOf (WitnessField c)
  fromBool (Bool c) = toIntegral $ unPar1 $ witnessF c
