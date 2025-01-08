{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module ZkFold.Symbolic.Data.MerkleTree where

import           ZkFold.Symbolic.Data.Class
import           GHC.Generics                     hiding (Rep)
import           ZkFold.Symbolic.Data.Payloaded   (Payloaded (..))
import           Data.Functor.Rep                 (Representable, distributeRep, Rep)
import           Data.Functor                     (Functor (..))
import           GHC.TypeNats
import           Data.Distributive                (Distributive (..))
import           ZkFold.Base.Data.Vector
import           Data.Type.Equality               (type (~))
import           Prelude                          (const, ($), undefined)
import           ZkFold.Symbolic.Data.Maybe
import           ZkFold.Symbolic.Data.Bool        (Bool, true)
import           Data.Foldable                    (Foldable (foldr))
import           ZkFold.Symbolic.Data.Input       (SymbolicInput, isValid)
import           ZkFold.Symbolic.Data.Conditional (bool, Conditional)
import           Control.DeepSeq                  (NFData)
import           ZkFold.Base.Data.ByteString      (Binary)
import           Control.Monad.Representable.Reader (WrappedRep)

newtype Leaf l a = Leaf { leafHash :: l a }
  deriving (Functor, Generic1)
instance (Representable l) => Distributive (Leaf l) where
  distribute :: (Functor f) => f (Leaf l a) -> Leaf l (f a)
  distribute = distributeRep

instance (Representable l) => Representable (Leaf l)


data MerkleTree (d :: Natural) a = MerkleTree {
    mHash     :: (Context a) (Layout a)
  , mLeaves   :: Payloaded (Vector (2 ^ d) :.: Leaf (Layout a)) (Context a)
  }
  deriving (Generic)

instance (SymbolicData a, KnownNat (2 ^ d)) => SymbolicData (MerkleTree d a)
instance
  ( SymbolicData a, SymbolicInput a
  , NFData (Rep (Layout a))
  , Binary (Rep (Layout a))
  , KnownNat (2 ^ d)
  , Binary (WrappedRep (Layout a)))
  => SymbolicInput (MerkleTree d a) where
    isValid _ = true

-- ZkFold.Symbolic.Data.Maybe
-- | Finds an element satisfying the constraint
find :: forall a d c.  (Conditional (Bool c) a, Foldable (MerkleTree d), SymbolicInput a, Context a ~ c) =>(a -> Bool c) -> MerkleTree d a -> Maybe c a
find p = let n = nothing @a @c in
  foldr (\i r -> maybe @a @_ @c (bool @(Bool c) n (just  @c i) $ p i) (const r) r) n


newtype MerkleTreePath (d :: Natural) a = MerkleTreePath { path :: Vector d (Bool (Context a))}
  deriving (Generic)

-- | Finds a path to an element satisfying the constraint
findPath :: (x -> Bool (Context x)) -> MerkleTree d x -> Maybe (Context x) (MerkleTreePath d x)
findPath _ = undefined

-- -- | Returns the element corresponding to a path
-- lookup :: MerkleTree d x -> MerkleTreePath d x -> x

-- -- | Inserts an element at a specified position in a tree
-- insert :: MerkleTree d x -> MerkleTreePath d x -> x -> MerkleTree d x

-- -- | Replaces an element satisfying the constraint. A composition of `findPath` and `insert`
-- replace :: (x -> Bool (Context x)) ->  MerkleTree d x -> x -> MerkleTree d x

-- -- | Returns the next path in a tree
-- incrementPath :: MerkleTreePath d x -> MerkleTreePath d x