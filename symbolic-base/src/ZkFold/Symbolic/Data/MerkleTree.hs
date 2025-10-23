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
) where

import Data.Bool (otherwise)
import Data.Foldable (foldl', foldr, toList)
import Data.Function (on, ($), (.))
import Data.Functor (fmap, (<$>))
import Data.Ord ((<=))
import Data.Type.Equality (type (~))
import qualified Data.Vector as Data
import Data.Zip (zipWith)
import GHC.Generics (Generic, Generic1, (:*:) (..), (:.:) (..))
import GHC.TypeLits (KnownNat, type (-), type (^))
import Test.QuickCheck (Arbitrary (..))
import Text.Show (Show)
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Collect (Collect (..))
import ZkFold.Data.Eq (Eq (..))
import qualified ZkFold.Data.MerkleTree as Base
import ZkFold.Data.Product (toPair)
import ZkFold.Data.Vector (Vector, mapWithIx, reverse, toV, unsafeToVector)
import ZkFold.Symbolic.Class (Arithmetic, Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (..), Conditional, assert, bool, (||))
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.FieldElement (FieldElement (FieldElement), fromFieldElement)
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import ZkFold.Symbolic.Data.Maybe (Maybe, fromJust, guard, mmap)
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import ZkFold.Symbolic.Data.Witness (Witness (Witness, witness))

data MerkleTree d c = MerkleTree
  { mHash :: FieldElement c
  , mLeaves :: Base.Leaves d (Witness c)
  }
  deriving (Generic, Generic1, Show, SymbolicData)

instance SymbolicInput (MerkleTree d) where
  isValid = isValid . mHash

instance Symbolic c => Collect (ConstrainedDatum c) (MerkleTree d c) where
  collect = collect . mHash

instance Symbolic c => Eq (MerkleTree d c) where
  type BooleanOf (MerkleTree d c) = Bool c
  (==) = (==) `on` mHash

emptyTree :: (KnownNat (Base.MerkleTreeSize d), Symbolic c) => MerkleTree d c
emptyTree = fromLeaves zero

unconstrainedFromLeaves
  :: Symbolic c => Base.Leaves d (Witness c) -> MerkleTree d c
unconstrainedFromLeaves mLeaves =
  let mHash = fromBaseHash (Base.computeRoot mLeaves) in MerkleTree {..}

fromLeaves :: Symbolic c => Base.Leaves d (FieldElement c) -> MerkleTree d c
fromLeaves src@(fmap toBaseHash -> mLeaves) =
  let mHash = Base.computeRoot src in MerkleTree {..}

toLeaves :: Symbolic c => MerkleTree d c -> Base.Leaves d (FieldElement c)
toLeaves src@MerkleTree {..} =
  unComp1 $
    assert ((== src) . fromLeaves . unComp1) $
      Comp1 $
        fromBaseHash <$> mLeaves

instance
  (Symbolic c, FromConstant a (FieldElement c), Base.MerkleTreeSize d ~ n)
  => FromConstant (Vector n a) (MerkleTree d c)
  where
  fromConstant = fromLeaves . fmap fromConstant

instance Arithmetic a => ToConstant (MerkleTree d a) where
  type Const (MerkleTree d a) = Base.Leaves d a
  toConstant = fmap (witness . toBaseHash) . toLeaves

instance
  (Arbitrary a, Arithmetic a, KnownNat (Base.MerkleTreeSize d))
  => Arbitrary (MerkleTree d a)
  where
  arbitrary = fromLeaves <$> arbitrary

type MerklePath d = Vector (d - 1) :.: (Bool :*: FieldElement)

type Index d = Vector (d - 1) :.: Bool

merklePath
  :: (Symbolic c, KnownNat (d - 1))
  => MerkleTree d c
  -> Index d c
  -> MerklePath d c
merklePath MerkleTree {..} position =
  let baseTree = Base.MerkleTree (toBaseHash mHash) mLeaves
      path = fromBaseHash <$> Base.merkleProve' baseTree (toBasePosition position)
   in Comp1 $ zipWith (:*:) (reverse $ unComp1 position) path

rootOnReplace
  :: Symbolic c
  => MerklePath d c -> FieldElement c -> FieldElement c
rootOnReplace (Comp1 path) value =
  foldl' ((. toPair) . Base.hashWithSibling) value path

data MerkleEntry d c = MerkleEntry
  { position :: Index d c
  , value :: FieldElement c
  }
  deriving (Generic, Generic1, SymbolicData, SymbolicInput)

contains
  :: forall d c
   . (Symbolic c, KnownNat (d - 1))
  => MerkleTree d c
  -> MerkleEntry d c
  -> Bool c
tree `contains` MerkleEntry {..} =
  rootOnReplace (merklePath tree position) value == mHash tree

(!!)
  :: forall d c
   . (Symbolic c, KnownNat (d - 1))
  => MerkleTree d c
  -> Index d c
  -> FieldElement c
tree !! position =
  assert (\value -> tree `contains` MerkleEntry {..}) $
    fromBaseHash $
      recIndex (fromBool <$> unComp1 position) $
        mLeaves tree
 where
  recIndex
    :: forall n b a. Conditional b a => Vector n b -> Vector (2 ^ n) a -> a
  recIndex i v = foldr splitter Data.head (toList i) (toV v)
   where
    splitter :: b -> (Data.Vector a -> a) -> Data.Vector a -> a
    splitter b rec d = let (l, r) = bisect d in ifThenElse b (rec r) (rec l)

  fromBool :: Bool c -> BooleanOf c
  fromBool (Bool b) = b == one

search
  :: forall c d
   . (Symbolic c, KnownNat (d - 1))
  => (FieldElement (Witness c) -> Bool (Witness c))
  -> MerkleTree d c
  -> Maybe (MerkleEntry d) c
search pred tree =
  assert (\entry -> tree `contains` fromJust entry) $
    toEntry $
      recSearch (fromBool . pred . FieldElement) (mLeaves tree)
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

  toEntry :: BooleanOf c ~ b => (b, Vector (d - 1) b, Witness c) -> Maybe (MerkleEntry d) c
  toEntry
    ( toBool -> wasFound
      , Comp1 . fmap toBool -> position
      , fromBaseHash -> value
      ) = guard wasFound MerkleEntry {..}

  fromBool :: Bool (Witness c) -> BooleanOf c
  fromBool ((Bool (((Witness b))))) = b == one

  toBool :: BooleanOf c -> Bool c
  toBool = Bool . bool zero one

find
  :: forall c d
   . (Symbolic c, KnownNat (d - 1))
  => (FieldElement (Witness c) -> Bool (Witness c))
  -> MerkleTree d c
  -> Maybe FieldElement c
find pred = mmap value . search pred

findIndex
  :: forall c d
   . (Symbolic c, KnownNat (d - 1))
  => (FieldElement (Witness c) -> Bool (Witness c))
  -> MerkleTree d c
  -> Maybe (Index d) c
findIndex pred = mmap position . search pred

elemIndex
  :: forall c d
   . (Symbolic c, KnownNat (d - 1))
  => FieldElement (Witness c)
  -> MerkleTree d c
  -> Maybe (Index d) c
elemIndex elem = findIndex (== elem)

lookup
  :: (Symbolic c, KnownNat (d - 1))
  => MerkleTree d c
  -> Index d c
  -> FieldElement c
lookup = (!!)

search'
  :: (Symbolic c, KnownNat (d - 1))
  => ( forall e
        . (Symbolic e, Order e ~ Order c)
       => FieldElement e -> Bool e
     )
  -> MerkleTree d c
  -> MerkleEntry d c
search' p = assert (p . value) . fromJust . search p

type KnownMerkleTree d = (KnownNat (d - 1), KnownNat (Base.MerkleTreeSize d))

replace
  :: (Symbolic c, KnownMerkleTree d)
  => MerkleEntry d c
  -> MerkleTree d c
  -> MerkleTree d c
replace entry@MerkleEntry {..} =
  assert (`contains` entry)
    . unconstrainedFromLeaves
    . mapWithIx (replacer (toBasePosition position, toBaseHash value))
    . mLeaves
 where
  replacer
    :: (FromConstant n i, Eq i, Conditional (BooleanOf i) a)
    => (i, a)
    -> n
    -> a
    -> a
  replacer (i, a') n = ifThenElse (i == fromConstant n) a'

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

fromBaseHash :: Witness c -> FieldElement c
fromBaseHash = FieldElement . witness

toBaseHash :: FieldElement c -> Witness c
toBaseHash = Witness . fromFieldElement

toBasePosition
  :: forall c d. Symbolic c => Index d c -> IntegralOf c
toBasePosition = foldl' (\x b -> double x + fromBool b) zero . unComp1
 where
  fromBool :: Bool c -> IntegralOf c
  fromBool (Bool b) = toIntegral b
