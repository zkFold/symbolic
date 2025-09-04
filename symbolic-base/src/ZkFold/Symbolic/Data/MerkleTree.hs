{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.MerkleTree where

import Control.Applicative (pure)
import Data.Foldable (foldl')
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Tuple (fst, uncurry)
import Data.Type.Equality (type (~))
import GHC.Generics (Generic, Generic1, Par1 (Par1, unPar1))
import GHC.TypeLits (KnownNat, type (-), type (^))
import Test.QuickCheck (Arbitrary (..))

import ZkFold.Algebra.Class
import ZkFold.Data.Eq (BooleanOf, Eq, (==))
import qualified ZkFold.Data.MerkleTree as Base
import ZkFold.Data.Package (packed)
import ZkFold.Data.Vector (Vector, zip)
import ZkFold.Symbolic.Class (BaseField, Symbolic (witnessF), WitnessField, embedW, fromCircuit2F)
import ZkFold.Symbolic.Data.Bool (Bool (..), Conditional, (&&))
import ZkFold.Symbolic.Data.Class (SymbolicData, arithmetize, payload, restore)
import ZkFold.Symbolic.Data.FieldElement (FieldElement (FieldElement), fieldElements, fromFieldElement)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Payloaded (Payloaded (runPayloaded), payloaded, restored)
import ZkFold.Symbolic.Data.Vec (Vec (..))
import ZkFold.Symbolic.MonadCircuit (IntegralOf, ResidueField (toIntegral), constraint)
import ZkFold.Symbolic.WitnessContext (WitnessContext (..))

type Leaves d = Vector (Base.MerkleTreeSize d)

data MerkleTree d c = MerkleTree
  { rootHash :: FieldElement c
  , leafHash :: Payloaded (Leaves d) FieldElement c
  }
  deriving (Eq, Generic, Generic1, SymbolicData, SymbolicInput)

toBaseLeaves :: Payloaded (Leaves d) FieldElement c -> Leaves d (WitnessField c)
toBaseLeaves = fmap (unPar1 . fst) . runPayloaded

emptyTree :: (KnownNat (Base.MerkleTreeSize d), Symbolic c) => MerkleTree d c
emptyTree = fromLeaves zero

assert :: (Symbolic c, SymbolicData h) => (h c -> Bool c) -> h c -> h c
assert p x =
  restore
    ( fromCircuit2F (arithmetize $ p x) (arithmetize x) \(Par1 b) i -> do
        constraint (\v -> v b - one)
        pure i
    , payload x
    )

fromLeaves :: Symbolic c => Vec (Leaves d) c -> MerkleTree d c
fromLeaves (fieldElements . runVec -> src@(payloaded -> leafHash)) =
  MerkleTree {..}
 where
  rootHash = Base.computeRoot src

toLeaves :: Symbolic c => MerkleTree d c -> Vec (Leaves d) c
toLeaves src@MerkleTree {..} =
  assert ((== src) . fromLeaves) $
    Vec $
      packed $
        fromFieldElement <$> restored leafHash

instance
  (Symbolic c, Arbitrary (BaseField c), KnownNat (Base.MerkleTreeSize d))
  => Arbitrary (MerkleTree d c)
  where
  arbitrary = fromLeaves <$> arbitrary

type Index d c = Vector (d - 1) (Bool c)

type Path d c = Vector (d - 1) (FieldElement c)

data MerkleEntry d c = MerkleEntry
  { position :: Index d c
  , value :: FieldElement c
  }
  deriving (Generic1, SymbolicData)

contains
  :: forall d c
   . (Symbolic c, KnownNat (d - 1))
  => MerkleTree d c -> MerkleEntry d c -> Bool c
tree `contains` MerkleEntry {..} =
  let path =
        FieldElement . embedW . Par1
          <$> Base.merkleProve'
            (toBaseTree tree)
            (foldl' (\x b -> double x + fromBool b) zero position)
   in foldl' Base.hashWithSibling value (zip position path) == rootHash tree
 where
  toBaseTree :: MerkleTree d c -> Base.MerkleTree d (WitnessField c)
  toBaseTree
    ( MerkleTree
        (unPar1 . witnessF . fromFieldElement -> mHash)
        (toBaseLeaves -> mLeaves)
      ) =
      Base.MerkleTree {..}

  fromBool :: Bool c -> IntegralOf (WitnessField c)
  fromBool (Bool c) = toIntegral $ unPar1 $ witnessF c

search
  :: forall c d
   . (Symbolic c, KnownNat (d - 1))
  => ( forall e
        . (Symbolic e, BaseField e ~ BaseField c)
       => FieldElement e -> Bool e
     )
  -> MerkleTree d c
  -> MerkleEntry d c
search pred tree =
  assert (\entry -> contains tree entry && pred (value entry)) $
    uncurry toEntry $
      recSearch
        (fromBool . pred . FieldElement . WC . Par1)
        (toBaseLeaves $ leafHash tree)
 where
  recSearch
    :: (Conditional b b, Conditional b a)
    => (a -> b) -> Vector (2 ^ n) a -> (Vector n b, a)
  recSearch p d = _

  toEntry
    :: Vector (d - 1) (BooleanOf (IntegralOf (WitnessField c)))
    -> WitnessField c
    -> MerkleEntry d c
  toEntry = _

  fromBool
    :: Bool (WitnessContext c) -> BooleanOf (IntegralOf (WitnessField c))
  fromBool (Bool b) = _

replace
  :: (Symbolic c, KnownNat (d - 1))
  => MerkleEntry d c -> MerkleTree d c -> MerkleTree d c
replace entry = assert (`contains` entry) . _ . leafHash
