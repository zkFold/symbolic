{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module ZkFold.Symbolic.Data.MerkleTree where

import           ZkFold.Symbolic.Data.Class
import           GHC.Generics                     hiding (Rep)
import           ZkFold.Symbolic.Data.Payloaded   (Payloaded (..))
import           Data.Functor.Rep                 (Representable, distributeRep, pureRep)
import           Data.Functor                     (Functor (..))
import           GHC.TypeNats
import           Data.Distributive                (Distributive (..))
import           ZkFold.Base.Data.Vector
import           Data.Type.Equality               (type (~))
import           Prelude                          (const, ($), undefined)
import           ZkFold.Symbolic.Data.Maybe
import           ZkFold.Symbolic.Data.Bool        (Bool)
import           Data.Foldable                    (Foldable (foldr))
import           ZkFold.Symbolic.Data.Input       (SymbolicInput)
import           ZkFold.Symbolic.Data.Conditional (bool, Conditional)
import ZkFold.Symbolic.Class
import ZkFold.Base.Algebra.Basic.Class

newtype Leaf a = Leaf { leafHash :: a }
  deriving (Functor, Generic1)

instance Distributive Leaf where
  distribute :: (Functor f) => f (Leaf a) -> Leaf (f a)
  distribute = distributeRep

instance Representable Leaf

data MerkleTree (d :: Natural) a = MerkleTree {
    mHash     :: Context a (Layout a)
  , mLeaves   :: Payloaded (Vector (2 ^ d) :.: Leaf) (Context a)
  }
  deriving (Generic)

instance (SymbolicData a, KnownNat (2 ^ d)) => SymbolicData (MerkleTree d a)
instance (SymbolicInput a, KnownNat (2 ^ d)) => SymbolicInput (MerkleTree d a)


-- ZkFold.Symbolic.Data.Maybe
-- | Finds an element satisfying the constraint
find :: forall a d c.  (Conditional (Bool c) a, Foldable (MerkleTree d), SymbolicInput a, Context a ~ c) => (a -> Bool c) -> MerkleTree d a -> Maybe c a
find p = let n = nothing @a @c in
  foldr (\i r -> maybe @a @_ @c (bool @(Bool c) n (just  @c i) $ p i) (const r) r) n


newtype MerkleTreePath (d :: Natural) a = MerkleTreePath { path :: Vector d (Bool (Context a))}
  deriving (Generic)

-- | Finds a path to an element satisfying the constraint
findPath :: (x -> Bool (Context x)) -> MerkleTree d x -> Maybe (Context x) (MerkleTreePath d x)
findPath p (MerkleTree h ls) = undefined -- MerkleTree h ls

-- -- | Returns the element corresponding to a path
-- lookup :: MerkleTree d x -> MerkleTreePath d x -> x

-- | Inserts an element at a specified position in a tree
insert ::
  ( Symbolic (Context a)
  , Representable (Layout a))
  =>MerkleTree d a -> MerkleTreePath d a -> a -> MerkleTree d a
insert (MerkleTree h ls) p x = MerkleTree (embed $ pureRep zero) ls

-- -- | Replaces an element satisfying the constraint. A composition of `findPath` and `insert`
-- replace :: (x -> Bool (Context x)) ->  MerkleTree d x -> x -> MerkleTree d x

-- -- | Returns the next path in a tree
-- incrementPath :: MerkleTreePath d x -> MerkleTreePath d x