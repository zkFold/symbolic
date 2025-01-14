{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module ZkFold.Symbolic.Data.MerkleTree where

import           ZkFold.Symbolic.Data.Class
import           GHC.Generics                     hiding (Rep)
import           Data.Functor.Rep                 (Representable, pureRep)
import           GHC.TypeNats
import           Data.Type.Equality               (type (~))
import           Prelude                          (const, ($), undefined, Traversable, Integer, (==))
import qualified Prelude                          as P
import           ZkFold.Symbolic.Data.Bool        (Bool (..), BoolType (..))
import           ZkFold.Symbolic.Data.Bool        (Bool (..))
import           Data.Foldable                    (Foldable (..))
import           ZkFold.Symbolic.Data.Input       (SymbolicInput)
import           ZkFold.Symbolic.Data.Conditional (bool, Conditional)
import ZkFold.Symbolic.Class
import ZkFold.Base.Algebra.Basic.Class
import ZkFold.Symbolic.Data.List (List, uncons, emptyList, (.:))
import ZkFold.Base.Data.Vector (knownNat)
import ZkFold.Symbolic.Data.Maybe (Maybe, just, nothing, maybe)
import ZkFold.Base.Protocol.IVC.Accumulator (x)

data MerkleTree (d :: Natural) a = MerkleTree {
    mHash     :: (Context a) (Layout a)
  , mLeaves   :: List (Context a) a
  }
  deriving (Generic)

instance (SymbolicData a, KnownNat (2 ^ d)) => SymbolicData (MerkleTree d a)
instance (SymbolicInput a, KnownNat (2 ^ d)) => SymbolicInput (MerkleTree d a)

instance
  ( Symbolic c
  , Context a ~ c
  , Traversable (Layout a)
  , Representable (Layout a)
  , Conditional (Bool c) (List c a)
  ) => Conditional (Bool c) (MerkleTree d a)

-- | Finds an element satisfying the constraint
find :: forall a d c.
  ( Conditional (Bool c) a
  , Foldable (MerkleTree d)
  , SymbolicInput a
  , Context a ~ c)
  => (a -> Bool c) -> MerkleTree d a -> Maybe c a
find p = let n = nothing @a @c in
  foldr (\i r -> maybe @a @_ @c (bool @(Bool c) n (just @c i) $ p i) (const r) r) n

newtype MerkleTreePath (d :: Natural) b = MerkleTreePath { mPath ::  List (Context b) b}
  deriving (Generic)

instance
  ( Symbolic c
  , c ~ Context b
  , KnownNat d
  , Conditional (Bool c) (List c b)
  ) => Conditional (Bool c) (MerkleTreePath d b)

-- | Finds a path to an element satisfying the constraint
findPath :: forall x c d.
  ( Context x ~ c
  , SymbolicOutput x
  , Conditional (Bool c) (List c x)
  , Conditional (Bool c) Integer
  , KnownNat d
  , Conditional P.Bool (List c (Bool c)))
  => (x -> Bool (Context x)) -> MerkleTree d x -> Maybe c (MerkleTreePath d (Bool c))
findPath p (MerkleTree _ ml) = just @c $ MerkleTreePath (bool path (emptyList @c) (ind P.== 0))
  where
    (ind, _) = helper (0, ml)

    helper :: (Integer, List c x) -> (Integer , List c x)
    helper (i, leaves) = let (l, ls) = uncons @c @x leaves
      in bool (helper (i + 1, ls)) (i, leaves) (p l)

    d = knownNat @d :: Integer

    path :: List c (Bool c)
    path = foldl (\nl ni -> Bool (embed (Par1 $ fromConstant ni)) .: nl) (emptyList @c)
      $ P.map (\i -> mod (div ind (2 P.^ i)) 2) [0 .. d]


-- | Returns the element corresponding to a path
lookup :: forall x c d.
  ( KnownNat d
  , SymbolicOutput x
  , Context x ~ c, Conditional (Bool c) Integer
  ) => MerkleTree d x -> MerkleTreePath d (Bool c) -> x
lookup (MerkleTree _ ml) (MerkleTreePath p) = val ml $ ind d 0 p
  where
    d = knownNat @d :: Integer

    ind :: Integer -> Integer -> List c (Bool c) -> Integer
    ind 0 i _ = i
    ind iter i ps = let (l, ls) = uncons @c ps
      in bool (ind (iter-1) (2*i) ls) (ind (iter-1) (2*i+1) ls) l

    val :: List c x -> Integer -> x
    val mt 0 = let (l, _) = uncons @c @x mt in l
    val mt i = let (_, ls) = uncons @c @x mt in val ls (i-1)


-- | Inserts an element at a specified position in a tree
insert ::
  ( Symbolic (Context a)
  , Representable (Layout a))
  => MerkleTree d a -> MerkleTreePath d a -> a -> MerkleTree d a
insert (MerkleTree h ls) p x = MerkleTree (embed $ pureRep zero) ls

-- -- | Replaces an element satisfying the constraint. A composition of `findPath` and `insert`
-- replace :: (x -> Bool (Context x)) ->  MerkleTree d x -> x -> MerkleTree d x

-- | Returns the next path in a tree
incrementPath :: MerkleTreePath d x -> MerkleTreePath d x
incrementPath = undefined