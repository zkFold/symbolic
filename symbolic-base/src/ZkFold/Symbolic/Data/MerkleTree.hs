{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module ZkFold.Symbolic.Data.MerkleTree where

import           Data.Foldable                    (Foldable (..), foldlM)
import           Data.Functor.Rep                 (Representable, pureRep)
import qualified Data.List                        as LL
import           Data.Proxy                       (Proxy (Proxy))
import           Data.Type.Equality               (type (~))
import           GHC.Generics                     hiding (Rep, UInt)
import           GHC.TypeNats
import           Prelude                          (Integer, Traversable, const, return, zip, ($), (.))
import qualified Prelude                          as P

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Control.HApplicative (hpair)
import           ZkFold.Base.Data.Package
import qualified ZkFold.Base.Data.Vector          as V
import           ZkFold.Base.Data.Vector
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool        (Bool (..))
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto), horner, mzipWithMRep)
import           ZkFold.Symbolic.Data.Conditional (Conditional, bool)
import           ZkFold.Symbolic.Data.Input       (SymbolicInput)
import qualified ZkFold.Symbolic.Data.List        as L
import           ZkFold.Symbolic.Data.List
import           ZkFold.Symbolic.Data.Maybe       (Maybe, just)
import           ZkFold.Symbolic.Data.UInt        (UInt, strictConv)
import           ZkFold.Symbolic.MonadCircuit

data MerkleTree (d :: Natural) h = MerkleTree {
    mHash   :: (Context h) (Layout h)
  , mLevels :: Vector d (List (Context h) h)
  }
  deriving (Generic)

hashAux :: forall i m a w f. (MonadCircuit i a w m, Representable f, Traversable f) => Par1 i -> f i -> f i -> m (f i)
hashAux (Par1 b) h g = do
  v1 <- merkleHasher h g
  v2 <- merkleHasher g h
  mzipWithMRep (\wx wy -> newAssigned ((one - ($ b)) * ($ wx) + ($ b) * ($ wy))) v1 v2
  where
    merkleHasher :: f i -> f i -> m (f i)
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
-- find :: forall h d c.
--   ( Conditional (Bool c) h
--   , SymbolicInput h
--   , Context h ~ c
--   , Foldable (List c))
--   => (h -> Bool c) -> MerkleTree d h -> Maybe c h
-- find p MerkleTree{..} = let n = nothing @h @c in
--   foldr (\i r -> maybe @h @_ @c (bool @(Bool c) n (just @c i) $ p i) (const r) r) n $ V.last mLevels

newtype MerkleTreePath (d :: Natural) c = MerkleTreePath { mPath :: Vector d (Bool c)}
  deriving (Generic)

instance ( Symbolic c, KnownNat d) => Conditional (Bool c) (MerkleTreePath d c)

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
  , KnownNat (d - 1)
  , KnownNat d
  ) => MerkleTree d x -> MerkleTreePath (d-1) c -> x
lookup (MerkleTree root nodes) (MerkleTreePath p) = xA
  where
    xP = leaf @c @x @d (V.last nodes) $ ind path

    inits = V.unsafeToVector @(d-1) $ LL.unfoldr (\v -> if LL.null v then P.Nothing else P.Just (ind @_ @c (V.unsafeToVector v), LL.init v)) (V.fromVector path)

    cinds :: Vector (d-1) (c Par1)
    cinds = unpacked $ fromCircuit2F (pack path) (pack inits) $ \ps' is' -> do
      let ps = P.fmap unPar1 (unComp1 ps')
          is = V.unsafeToVector $ fromConstant @(BaseField c) zero : (P.init . V.fromVector $ P.fmap unPar1 (unComp1 is'))
      mzipWithMRep (\wp wi -> newAssigned (one - ($ wp) + ($ wi)*(one + one))) ps is

    inds = V.unsafeToVector @(d-1) $ P.zipWith (\l i -> arithmetize (leaf @c @x @d l i) Proxy) (V.fromVector $ V.tail nodes) (V.fromVector cinds)

    xA = restore @x @c $ const (preimage , payload xP Proxy)

    rootAndH1 :: Context x (Layout x :*: Layout x)
    rootAndH1 = hpair root (arithmetize xP Proxy)

    preimage :: c (Layout x)
    preimage = fromCircuit3F (pack inds) (pack path) rootAndH1 $ \ g p' r' -> do
      let gs = V.fromVector $ unComp1 g
          bs = V.fromVector $ unComp1 p'
          (r :*: h1) = r'
      hd :: Layout x i <- foldlM (\h' (g', b') -> hashAux b' h' g') h1 $ zip gs bs
      _ <- mzipWithMRep (\wx wy -> constraint (($ wx) - ($ wy))) hd r
      return h1

    path = P.fmap (\(Bool b) -> b) p


leaf :: forall c x d. (SymbolicOutput x, Context x ~ c, KnownNat d) => List c x -> c Par1 -> x
leaf l i = let num = strictConv @_ @(UInt d Auto c) i in l L.!! num


ind :: forall d c. (Symbolic c) => Vector d (c Par1) -> c Par1
ind vb = fromCircuitF (pack vb) $ \vb' -> do
  let bs = P.map unPar1 $ V.fromVector $ unComp1 vb'
  b1n <- P.fmap Par1 . horner $ bs
  return $ fromConstant b1n

-- | Inserts an element at a specified position in a tree
insert ::
  ( Symbolic (Context a)
  , Representable (Layout a))
  => MerkleTree d a -> MerkleTreePath d c -> a -> MerkleTree d a
insert (MerkleTree _ ls) _ _ = MerkleTree (embed $ pureRep zero) ls

-- -- | Replaces an element satisfying the constraint. A composition of `findPath` and `insert`
-- replace :: (x -> Bool (Context x)) ->  MerkleTree d x -> x -> MerkleTree d x

-- | Returns the next path in a tree
-- incrementPath :: forall c d.
--   ( KnownNat d
--   , Symbolic c
--   ) => MerkleTreePath d c -> MerkleTreePath d c
-- incrementPath (MerkleTreePath p) = MerkleTreePath (path $ ind p +one)
--   where
--     d = knownNat @d :: Integer

--     path :: Natural -> Vector d (Bool c)
--     path val = unsafeToVector $ foldl (\nl ni -> Bool (embed @c (Par1 $ fromConstant ni )) : nl) []
--       $ P.map (\i -> mod (div val (2 P.^ i)) 2) [1 .. d]
