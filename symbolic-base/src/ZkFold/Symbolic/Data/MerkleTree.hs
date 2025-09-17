{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.MerkleTree (
  MerkleTree,
  emptyTree,
  fromLeaves,
  toLeaves,
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
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Ord ((<=))
import Data.Tuple (fst)
import Data.Type.Equality (type (~))
import qualified Data.Vector as Data
import Data.Zip (zipWith)
import GHC.Generics (Generic, Generic1, Par1 (Par1, unPar1), U1 (..), (:.:) (..))
import GHC.TypeLits (KnownNat, type (-), type (^))
import Test.QuickCheck (Arbitrary (..))
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq (BooleanOf, Eq, (==))
import ZkFold.Data.HFunctor.Classes (HEq, HShow)
import qualified ZkFold.Data.MerkleTree as Base
import ZkFold.Data.Package (packed)
import ZkFold.Data.Vector (Vector, mapWithIx, reverse, toV, unsafeToVector, zip)
import ZkFold.Symbolic.Class (Arithmetic, BaseField, Symbolic, WitnessField, embedW, witnessF)
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType, Conditional, assert, bool, (||))
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.FieldElement (FieldElement (FieldElement), fieldElements, fromFieldElement)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Maybe (Maybe, fromJust, guard, mmap)
import ZkFold.Symbolic.Data.Payloaded (Payloaded (..), payloaded, restored)
import ZkFold.Symbolic.Data.Vec (Vec (..))
import ZkFold.Symbolic.Interpreter (Interpreter (runInterpreter))
import ZkFold.Symbolic.MonadCircuit (IntegralOf, toIntegral)
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

unconstrainedFromLeaves
  :: Symbolic c => Base.Leaves d (WitnessField c) -> MerkleTree d c
unconstrainedFromLeaves src@(Payloaded . fmap ((,U1) . Par1) -> mLeaves) =
  MerkleTree {..}
 where
  mHash = fromBaseHash (Base.computeRoot src)

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

type Index d = Vector (d - 1) :.: Bool

data MerkleEntry d c = MerkleEntry
  { position :: Index d c
  , value :: FieldElement c
  }
  deriving (Generic1, SymbolicData)

contains
  :: forall d c
   . (Symbolic c, KnownNat (d - 1))
  => MerkleTree d c -> MerkleEntry d c -> Bool c
MerkleTree {..} `contains` MerkleEntry {..} =
  let baseTree = Base.MerkleTree (toBaseHash mHash) (toBaseLeaves mLeaves)
      path = fromBaseHash <$> Base.merkleProve' baseTree (toBasePosition position)
   in foldl' Base.hashWithSibling value (zip (reverse (unComp1 position)) path) == mHash

type Bool' c = BooleanOf (IntegralOf (WitnessField c))

(!!)
  :: forall d c
   . (Symbolic c, KnownNat (d - 1))
  => MerkleTree d c -> Index d c -> FieldElement c
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
   . (Symbolic c, KnownNat (d - 1))
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
    => (a -> b) -> Vector (2 ^ n) a -> (b, Vector n b, a)
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
   . (Symbolic c, KnownNat (d - 1))
  => (FieldElement (WitnessContext c) -> Bool (WitnessContext c))
  -> MerkleTree d c
  -> Maybe FieldElement c
find pred = mmap value . search pred

findIndex
  :: forall c d
   . (Symbolic c, KnownNat (d - 1))
  => (FieldElement (WitnessContext c) -> Bool (WitnessContext c))
  -> MerkleTree d c
  -> Maybe (Index d) c
findIndex pred = mmap position . search pred

elemIndex
  :: forall c d
   . (Symbolic c, KnownNat (d - 1))
  => FieldElement (WitnessContext c)
  -> MerkleTree d c
  -> Maybe (Index d) c
elemIndex elem = findIndex (== elem)

lookup
  :: (Symbolic c, KnownNat (d - 1))
  => MerkleTree d c -> Index d c -> FieldElement c
lookup = (!!)

search'
  :: (Symbolic c, KnownNat (d - 1))
  => (forall e. (Symbolic e, BaseField e ~ BaseField c) => FieldElement e -> Bool e)
  -> MerkleTree d c
  -> MerkleEntry d c
search' p = assert (p . value) . fromJust . search p

type KnownMerkleTree d = (KnownNat (d - 1), KnownNat (Base.MerkleTreeSize d))

replace
  :: (Symbolic c, KnownMerkleTree d)
  => MerkleEntry d c -> MerkleTree d c -> MerkleTree d c
replace entry@MerkleEntry {..} =
  assert (`contains` entry)
    . unconstrainedFromLeaves
    . mapWithIx (replacer (toBasePosition position, toBaseHash value))
    . toBaseLeaves
    . mLeaves
 where
  replacer
    :: (FromConstant n i, Eq i, Conditional (BooleanOf i) a)
    => (i, a) -> n -> a -> a
  replacer (i, a') n = ifThenElse (i == fromConstant n) a'

replaceAt
  :: (Symbolic c, KnownMerkleTree d)
  => Index d c -> FieldElement c -> MerkleTree d c -> MerkleTree d c
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
