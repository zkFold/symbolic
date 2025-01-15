{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module ZkFold.Symbolic.Data.MerkleTree where

import           Data.Foldable                    (Foldable (..))
import           Data.Functor.Rep                 (Representable, pureRep)
import           Data.Type.Equality               (type (~))
import           GHC.Generics                     hiding (Rep)
import           GHC.TypeNats
import           Prelude                          (Integer, Traversable, const, ($), return)
import qualified Prelude                          as P

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Data.Vector
import qualified ZkFold.Base.Data.Vector as V
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool        (Bool (..))
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Conditional (Conditional, bool)
import           ZkFold.Symbolic.Data.Input       (SymbolicInput)
import           ZkFold.Symbolic.Data.List
import qualified ZkFold.Symbolic.Data.List as L
import           ZkFold.Symbolic.Data.Maybe       (Maybe, just, maybe, nothing)
import ZkFold.Symbolic.MonadCircuit
import Data.Traversable (Traversable(traverse))
import ZkFold.Base.Control.HApplicative (hunit)
import Data.Proxy (Proxy(Proxy))

data MerkleTree (d :: Natural) h = MerkleTree {
    mHash   :: (Context h) (Layout h)
  , mLevels :: Vector d (List (Context h) h)
  }
  deriving (Generic)

hashAux :: forall i m a w. MonadCircuit i a w m => i -> i -> i -> m i
hashAux b h g = newAssigned (($ b) * ($ merkleHasher h g) + (one - ($ b)* ($ merkleHasher g h)))
  where
    merkleHasher :: i -> i -> i
    merkleHasher = P.undefined

instance (SymbolicData h, KnownNat d) => SymbolicData (MerkleTree d h)
instance (SymbolicInput h, KnownNat d) => SymbolicInput (MerkleTree d h)

instance
  ( Symbolic c
  , Context h ~ c
  , Conditional (Bool c) (List c h)
  , Traversable (Layout h)
  , Representable (Layout h)
  , KnownNat d
  ) => Conditional (Bool c) (MerkleTree d h)

-- | Finds an element satisfying the constraint
find :: forall h d c.
  ( Conditional (Bool c) h
  , SymbolicInput h
  , Context h ~ c
  , Foldable (List c))
  => (h -> Bool c) -> MerkleTree d h -> Maybe c h
find p MerkleTree{..} = let n = nothing @h @c in
  foldr (\i r -> maybe @h @_ @c (bool @(Bool c) n (just @c i) $ p i) (const r) r) n $ V.last mLevels

newtype MerkleTreePath (d :: Natural) c = MerkleTreePath { mPath :: Vector d (Bool c)}
  deriving (Generic)

instance
  ( Symbolic c
  -- , c ~ Context b
  , KnownNat d
  -- , Conditional (Bool c) b
  -- , Conditional (Bool c) (List c b)
  ) => Conditional (Bool c) (MerkleTreePath d c)

-- | Finds a path to an element satisfying the constraint
findPath :: forall x c d.
  ( Context x ~ c
  , SymbolicOutput x
  , Symbolic c
  , Conditional (Bool c) (List c x)
  , Conditional (Bool c) Integer
  , KnownNat d)
  => (x -> Bool (Context x)) -> MerkleTree d x -> Maybe c (MerkleTreePath d c)
findPath p (MerkleTree _ mll) = just @c $ MerkleTreePath path
  where
    ml = V.last mll
    (ind', _) = helper (0, ml)

    helper :: (Integer, List c x) -> (Integer , List c x)
    helper (i, leaves) = let (l, ls) = L.uncons @c @x leaves
      in bool (helper (i + 1, ls)) (i, leaves) (p l)

    d = knownNat @d :: Integer

    path :: Vector d (Bool c)
    path = unsafeToVector $ foldl (\nl ni -> Bool (embed @c (Par1 $ fromConstant ni )) : nl) []
      $ P.map (\i -> mod (div ind' (2 P.^ i)) 2) [1 .. d]


-- | Returns the element corresponding to a path
lookup :: forall x c d.
  ( SymbolicOutput x
  , Context x ~ c
  , Conditional (Bool c) Integer
  ) => MerkleTree d x -> MerkleTreePath (d-1) c -> x
lookup (MerkleTree root nodes) (MerkleTreePath p) = xA
  where
    xP = leaf @x (V.last nodes) $ ind p

    xA = restore @x @c $ const
        ( preimage --fromCircuitF @c hunit $ const (traverse unconstrained (witnessF $ arithmetize xP Proxy) )
        , payload xP Proxy)

    preimage :: c (Layout x)
    preimage = fromCircuitVF path $ \p' -> do
      let Par1 i = P.head p'
      return i
      -- fromCircuitF @c hunit $ const (traverse unconstrained (witnessF $ arithmetize xP Proxy))

    path = P.map (\(Bool b) -> b) $ V.fromVector p -- pack $ V.toVector $ P.map embed (unsafeToVector p)

-- lookup (MerkleTree root hashAux leaves) (MerkleTreePath v) = let x = хэш искомой вершины in
--   fromCircuit3F v root x $ bs r h1-> do
--     gs <- traversal . unconstrained $ хэши-соседи к листам вдоль пути
--     r' <- foldl (\h' (g', b') -> newAssigned (hashAux b' h' g') h1 $ zip gs bs
--     constraint (r' - r)
--     return h1

leaf :: forall x. (SymbolicOutput x) => List (Context x) x -> Integer -> x
leaf mt i = let (l, ls) = L.uncons mt in
      case i of
        0 -> l
        _ -> leaf ls (i-1)

ind :: forall d c. (Conditional (Bool c) Integer) => Vector d (Bool c) -> Integer
ind ps = let ls = V.fromVector ps
      in foldl (\num b -> bool (num*2 +1) (num*2) b) 0 ls





-- | Inserts an element at a specified position in a tree
insert ::
  ( Symbolic (Context a)
  , Representable (Layout a))
  => MerkleTree d a -> MerkleTreePath d c -> a -> MerkleTree d a
insert (MerkleTree _ ls) _ _ = MerkleTree (embed $ pureRep zero) ls

-- -- | Replaces an element satisfying the constraint. A composition of `findPath` and `insert`
-- replace :: (x -> Bool (Context x)) ->  MerkleTree d x -> x -> MerkleTree d x

-- | Returns the next path in a tree
incrementPath :: forall c d x.
  ( KnownNat d
  , Context x ~ c
  , Symbolic c
  , Conditional (Bool c) Integer
  ) => MerkleTreePath d c -> MerkleTreePath d c
incrementPath (MerkleTreePath p) = MerkleTreePath (path $ ind p + 1)
  where
    d = knownNat @d :: Integer

    ind :: Vector d (Bool c) -> Integer
    ind ps = let ls = V.fromVector ps
      in foldl (\num b -> bool (num*2 +1) (num*2) b) 1 ls

    path :: Integer -> Vector d (Bool c)
    path val = unsafeToVector $ foldl (\nl ni -> Bool (embed @c (Par1 $ fromConstant ni )) : nl) []
      $ P.map (\i -> mod (div val (2 P.^ i)) 2) [1 .. d]


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
