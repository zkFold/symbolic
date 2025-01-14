{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module ZkFold.Symbolic.Data.MerkleTree where

import           Data.Foldable                    (Foldable (..))
import           Data.Functor.Rep                 (Representable, pureRep)
import           Data.List.Infinite               (Infinite (..))
import           Data.Type.Equality               (type (~))
import           GHC.Generics                     hiding (Rep)
import           GHC.TypeNats
import           Prelude                          (Integer, Traversable, const, undefined, ($))
import qualified Prelude                          as P

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Data.Vector          (knownNat)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool        (Bool (..))
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Conditional (Conditional, bool)
import           ZkFold.Symbolic.Data.Hash        (Hashable (hasher))
import           ZkFold.Symbolic.Data.Input       (SymbolicInput)
import           ZkFold.Symbolic.Data.List        (List (..), ListItem (..), emptyList, uncons, (.:))
import           ZkFold.Symbolic.Data.Maybe       (Maybe, just, maybe, nothing)
import           ZkFold.Symbolic.Data.Payloaded
import           ZkFold.Symbolic.MonadCircuit

data MerkleTree (d :: Natural) a = MerkleTree {
    mHash   :: (Context a) (Layout a)
  , mLeaves :: List (Context a) a
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
  , Context x ~ c
  , Conditional (Bool c) Integer
  ) => MerkleTree d x -> MerkleTreePath d (Bool c) -> x
lookup (MerkleTree _ ml) (MerkleTreePath p) = val ml $ ind d 0 p
  where
    d = knownNat @d :: Integer

    ind :: Integer -> Integer -> List c (Bool c) -> Integer
    ind 0 i _ = i
    ind iter i ps = let (l, ls) = uncons @c ps
      in bool (ind (iter-1) (2*i) ls) (ind (iter-1) (2*i+1) ls) l

    val :: List c x -> Integer -> x
    val mt i = let (l, ls) = uncons @c @x mt in
      case i of
        0 -> l
        _ -> val ls (i-1)


rollTree :: forall d a c.
  ( c ~ Context a
  , SymbolicOutput a
  , KnownNat d
  , Hashable a a
  , AdditiveSemigroup a
  ) => MerkleTree d a -> MerkleTree (d - 1) a
rollTree (MerkleTree h l) = MerkleTree h (solve d l)
  where
    d = 2 P.^ (knownNat @d :: Integer)

    solve :: Integer -> List c a -> List c a
    solve 0 _ = emptyList @c
    solve i lst =
      let (x1, list1) = uncons lst
          (x2, olist) = uncons list1
      in hasher (x1 + x2) .: solve (i - 2) olist



-- | Inserts an element at a specified position in a tree
insert ::
  ( Symbolic (Context a)
  , Representable (Layout a))
  => MerkleTree d a -> MerkleTreePath d a -> a -> MerkleTree d a
insert (MerkleTree h ls) p x = MerkleTree (embed $ pureRep zero) ls

-- -- | Replaces an element satisfying the constraint. A composition of `findPath` and `insert`
-- replace :: (x -> Bool (Context x)) ->  MerkleTree d x -> x -> MerkleTree d x

-- | Returns the next path in a tree
incrementPath :: forall c d.
  ( KnownNat d
  , Symbolic c
  , Conditional (Bool c) Integer
  ) => MerkleTreePath d (Bool c) -> MerkleTreePath d (Bool c)
incrementPath (MerkleTreePath p) = MerkleTreePath (path $ ind d 0 p + 1)
  where
    d = knownNat @d :: Integer

    ind :: Integer -> Integer -> List c (Bool c) -> Integer
    ind 0 i _ = i
    ind iter i ps = let (l, ls) = uncons @c ps
      in bool (ind (iter-1) (2*i) ls) (ind (iter-1) (2*i+1) ls) l

    path :: Integer -> List c (Bool c)
    path val = foldl (\nl ni -> Bool (embed (Par1 $ fromConstant ni)) .: nl) (emptyList @c)
      $ P.map (\i -> mod (div val (2 P.^ i)) 2) [0 .. d]


-- | Returns the previous path in a tree
-- decrementPath :: forall c d.
--   ( KnownNat d
--   , Symbolic c
--   , Conditional (Bool c) Integer
--   ) => MerkleTreePath d (Bool c) -> MerkleTreePath d (Bool c)
-- decrementPath (MerkleTreePath p) = MerkleTreePath (path $ ind d 0 p - 1)
--   where
--     d = knownNat @d :: Integer

--     ind :: Integer -> Integer -> List c (Bool c) -> Integer
--     ind 0 i _ = i
--     ind iter i ps = let (l, ls) = uncons @c ps
--       in bool (ind (iter-1) (2*i) ls) (ind (iter-1) (2*i+1) ls) l

--     path :: Integer -> List c (Bool c)
--     path val = foldl (\nl ni -> Bool (embed (Par1 $ fromConstant ni)) .: nl) (emptyList @c)
--       $ P.map (\i -> mod (div val (2 P.^ i)) 2) [0 .. d]
