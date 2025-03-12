{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module ZkFold.Symbolic.Data.MerkleTree where

import           Data.Constraint                                (withDict)
import           Data.Constraint.Nat                            (minusNat, plusMinusInverse3)
import           Data.Functor.Rep                               (pureRep)
import qualified Data.List                                      as LL
import           Data.Proxy                                     (Proxy (Proxy))
import           Data.Semialign                                 (Zip)
import           Data.Type.Equality                             (type (~))
import           Data.Vector                                    (iterateN)
import           GHC.Generics                                   hiding (Rep, UInt, from)
import           GHC.TypeNats
import           Prelude                                        (const, return, zip, ($), (.))
import qualified Prelude                                        as P

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Data.Package
import qualified ZkFold.Base.Data.Vector                        as V
import           ZkFold.Base.Data.Vector                        hiding ((.:))
import           ZkFold.Symbolic.Algorithms.Hash.MiMC
import           ZkFold.Symbolic.Algorithms.Hash.MiMC.Constants
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool                      (Bool (..), true)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators               (Iso (from), KnownRegisters, RegisterSize (Auto),
                                                                 expansion, getNatural, horner, mzipWithMRep,
                                                                 withNumberOfRegisters)
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.FieldElement              (FieldElement (FieldElement, fromFieldElement))
import           ZkFold.Symbolic.Data.Input                     (SymbolicInput)
import qualified ZkFold.Symbolic.Data.List                      as L
import           ZkFold.Symbolic.Data.List
import           ZkFold.Symbolic.Data.Maybe
import           ZkFold.Symbolic.Data.Morph
import           ZkFold.Symbolic.Data.Switch
import           ZkFold.Symbolic.Data.UInt                      (UInt (..), strictConv)
import           ZkFold.Symbolic.Data.Vec
import           ZkFold.Symbolic.Fold                           (SymbolicFold)
import           ZkFold.Symbolic.MonadCircuit


data MerkleTree (d :: Natural) h = MerkleTree {
    mHash   :: (Context h) (Layout h)
  , mLevels :: Vector d (List (Context h) h)
  }
  deriving (Generic)

-- | Сreates a layer above the current one in the merkle tree
layerFolding :: forall c x.
  (SymbolicOutput x, Context x ~ c, SymbolicFold c, Zip  (Layout x))
  => List c x -> List c x
layerFolding xs = res
  where
    (_, res) = foldl (Morph \((arr, l ), a :: Switch s x) ->
        ifThenElse (isNothing arr :: Bool s) (just a, l) (nothing, newVer (fromJust arr) a .: l)) (nothing :: Maybe c x, emptyList :: List c x) xs

    newVer ::
      forall s y. ( Symbolic s, SymbolicData y, Zip (Layout y))
      => Switch s y -> Switch s y -> Switch s y
    newVer l' r' = Switch (newLayout @s @y (sLayout l') (sLayout r')) (sPayload l')

    newLayout :: forall s y.
      ( Symbolic s, SymbolicData y, Zip (Layout y))
      => s (Layout y) -> s (Layout y) -> s (Layout y)
    newLayout = hashAux @s @y z
      where
        Bool z = true :: Bool s

instance forall c x d n.
  ( SymbolicFold c, SymbolicOutput x,
    Context x ~ c, Zip (Layout x), KnownNat d, 2 ^ (d-1) ~ n, 1 <= d
  ) => Iso (Vector n x) (MerkleTree d x) where
  from v = withDict (plusMinusInverse3 @1 @d) $ MerkleTree (bool (P.error "Invalid vector length") (arithmetize h Proxy) (L.null checkL)) (hl V..: ls)
    where
      (h, checkL) = L.uncons hl
      (hl :: List c x, ls :: Vector (d-1) (List c x)) =
        withDict (plusMinusInverse3 @1 @d) $
        V.uncons @d @(List c x). V.reverse . Vector $ iterateN (P.fromIntegral $ getNatural @d) layerFolding vs
      vs :: List c x = P.foldr (L..:) emptyList $ fromVector v

instance forall c x d n.
  ( SymbolicFold c, SymbolicOutput x,
    Context x ~ c, Zip (Layout x), KnownNat d, 2 ^ (d-1) ~ n, 1 <= d
  ) => Iso (MerkleTree d x) (Vector n x)  where
  from (MerkleTree _ l) = V.unsafeToVector $ helper @c (V.last l) (2 ^ (getNatural @d -! 1))
    where
      helper :: forall s y. (SymbolicOutput y, Symbolic s, Context y ~ s) => List s y -> Natural -> [y]
      helper ls k = case k of
        0 -> []
        _ -> let (n, ns) = L.uncons ls in n : helper @s ns (k-!1)

hashAux :: forall c x.
  (Symbolic c, SymbolicData x, Zip (Layout x))
  => c Par1 -> c (Layout x) -> c (Layout x) -> c (Layout x)
hashAux b' h g = do
  let (Vec v1) = merkleHasher [Vec h, Vec g]
      (Vec v2) = merkleHasher [Vec g, Vec h]
  fromCircuit3F b' v1 v2 $ \ (Par1 b) x y ->
    mzipWithMRep (\wx wy -> newAssigned ((one - ($ b)) * ($ wx) + ($ b) * ($ wy))) x y
  where
    merkleHasher :: [Vec (Layout x) c] -> Vec (Layout x) c
    merkleHasher = mimcHashN mimcConstants (zero :: BaseField c)

instance (SymbolicData h, KnownNat d) => SymbolicData (MerkleTree d h)
instance (SymbolicInput h, KnownNat d) => SymbolicInput (MerkleTree d h)

-- | Finds an element satisfying the constraint
find :: forall c h d.
  ( Conditional (Bool c) h, SymbolicInput h
  , Context h ~ c, SymbolicFold c
  ) => MorphFrom c h (Bool c) -> MerkleTree d h -> Maybe c h
find p MerkleTree{..} =
  let leaves = V.last mLevels
      arr = L.filter p leaves
  in bool (just $ L.head arr) nothing (L.null arr)

newtype MerkleTreePath (d :: Natural) c = MerkleTreePath { mPath :: Vector (d-1) (Bool c)}
  deriving (Generic)

instance (Symbolic c, KnownNat (d-1)) => SymbolicData (MerkleTreePath d c)
instance (Symbolic c, KnownNat (d-1)) => Conditional (Bool c) (MerkleTreePath d c)

-- | Finds a path to an element satisfying the constraint
findPath :: forall x c d n.
  ( SymbolicOutput x
  , Context x ~ c
  , SymbolicFold c
  , KnownNat d
  , 1 <= d
  , NumberOfBits (BaseField c) ~ n
  , Zip (Layout x)
  ) => MorphFrom c x (Bool c) -> MerkleTree d x -> Maybe c (MerkleTreePath d c)
findPath p mt@(MerkleTree _ nodes) = withDict (minusNat @d @1) $ bool (nothing @_ @c) (just path) (p @ lookup @x @c mt path :: Bool c)
  where
    leaves = V.last nodes
    path = withNumberOfRegisters @n @Auto @(BaseField c) $
      MerkleTreePath . P.fmap Bool . indToPath @c . fromFieldElement . from $ findIndex p leaves

indToPath :: forall c d. (Symbolic c, KnownNat d) => c Par1 -> Vector (d - 1) (c Par1)
indToPath e = unpack $ fromCircuitF e $ \(Par1 i) -> do
    ee <- expansion (knownNat @d) i
    return $ Comp1 (V.unsafeToVector @(d-1) $ P.map Par1 ee)

-- | Returns the element corresponding to a path
lookup :: forall x c d.
  ( SymbolicOutput x
  , Context x ~ c
  , SymbolicFold c
  , KnownNat d
  , 1 <= d
  , Zip (Layout x)
  ) => MerkleTree d x -> MerkleTreePath d c -> x
lookup (MerkleTree root nodes) (MerkleTreePath p) = xA
  where
    xP = leaf @c @x @d (V.last nodes) $ ind path

    -- element indices along the path on each layer
    inits = V.unsafeToVector @(d-2) $
              LL.unfoldr (\v -> let initV = LL.init v
                                 in if LL.null initV then P.Nothing else P.Just (ind @_ @c (V.unsafeToVector initV), initV)) $ V.fromVector path

    -- indices of the adjacent element along the path
    cinds :: Vector (d-1) (c Par1)
    cinds = unpacked $ fromCircuit2F (pack path) (pack inits) $ \ps' is' -> do
      let ps = P.fmap unPar1 (unComp1 ps')
          is = V.unsafeToVector @(d-1) (fromConstant @(BaseField c) zero : (V.fromVector . P.fmap unPar1 $ unComp1 is'))
      withDict (minusNat @d @1) $ mzipWithMRep (\wp wi -> newAssigned (one - ($ wp) + ($ wi)*(one + one))) ps is

    -- adjacent elements along paths
    pairs = V.unsafeToVector @(d-1) $ P.zipWith (\l i -> arithmetize (leaf @c @x @d l i) Proxy) (V.fromVector $ V.tail nodes) (V.fromVector cinds)

    xA = restore @x @c $ const (preimage , payload xP Proxy)

    preimage :: c (Layout x)
    preimage =
      let gs = V.fromVector pairs
          bs = V.fromVector path
          hd = P.foldl (\h' (g', b') -> hashAux @c @x b' h' g') (arithmetize xP Proxy) $ zip gs bs
       in fromCircuit2F hd root $ \a b -> do
        _ <- mzipWithMRep (\wx wy -> constraint (($ wx) - ($ wy))) a b
        return a

    path :: Vector (d - 1) (c Par1)
    path = P.fmap (\(Bool b) -> b) p

-- element by index
leaf :: forall c x d.
  ( SymbolicOutput x
  , Context x ~ c
  , SymbolicFold c
  , KnownNat d
  ) => List c x -> c Par1 -> x
leaf l i = withNumberOfRegisters @d @Auto @(BaseField c) $
  let num = strictConv @_ @(UInt d Auto c) i in l L.!! num

-- index of element in path to element
ind :: forall d c. (Symbolic c) => Vector d (c Par1) -> c Par1
ind vb = fromCircuitF (pack vb) $ \vb' -> do
  let bs = P.map unPar1 $ V.fromVector $ unComp1 vb'
  b1n <- P.fmap Par1 . horner $ bs
  return $ fromConstant b1n

-- | Inserts an element at a specified position in a tree
insertLeaf :: forall x c d.
  ( SymbolicOutput x
  , Context x ~ c
  , SymbolicFold c
  , KnownNat d
  , 1 <= d
  , KnownRegisters c d Auto
  , Zip (Layout x)
  ) => MerkleTree d x -> MerkleTreePath d c -> x -> MerkleTree d x
insertLeaf (MerkleTree _ nodes) (MerkleTreePath p) xI = MerkleTree (V.head preimage) (V.unsafeToVector z3)
  where
    -- element indices along the path on each layer
    inits = V.unsafeToVector @(d-2) $
              LL.unfoldr (\v -> let initV = LL.init v
                                 in if LL.null initV then P.Nothing else P.Just (ind @_ @c (V.unsafeToVector initV), initV)) $ V.fromVector path

    -- indices of the adjacent element along the path
    cinds :: Vector (d-1) (c Par1)
    cinds = unpacked $ fromCircuit2F (pack path) (pack inits) $ \ps' is' -> do
      let ps = P.fmap unPar1 (unComp1 ps')
          is = V.unsafeToVector @(d-1) (fromConstant @(BaseField c) zero : (V.fromVector . P.fmap unPar1 $ unComp1 is'))
      withDict (minusNat @d @1) $ mzipWithMRep (\wp wi -> newAssigned (one - ($ wp) + ($ wi)*(one + one))) ps is

    -- adjacent elements along paths
    pairs = V.unsafeToVector @(d-1) $ P.zipWith (\l i -> arithmetize (leaf @c @x @d l i) Proxy) (V.fromVector $ V.tail nodes) (V.fromVector cinds)

    preimage :: Vector (d - 1) (c (Layout x))
    preimage =
      let gs = V.fromVector pairs
          bs = V.fromVector path
       in  V.unsafeToVector @(d-1) $ helper (arithmetize xI Proxy) (zip gs bs)

    helper :: c (Layout x) -> [(c (Layout x), c Par1)] -> [c (Layout x)]
    helper _ [] =  []
    helper h (pi:ps) =
      let (g, b) = pi
          hN = hashAux @c @x b h g
      in hN : (helper hN ps)

    path :: Vector (d - 1) (c Par1)
    path = P.fmap (\(Bool b) -> b) p

    z3 = P.zipWith3 (\l mtp xi -> L.insert l mtp (restore @_ @c $ const (xi, pureRep zero)))
          (V.fromVector nodes)
          (P.map (strictConv @_ @(UInt d Auto c)) ([embed $ Par1 zero] P.++ V.fromVector inits P.++ [ind path]))
          (V.fromVector preimage P.<> [arithmetize xI Proxy])

-- | Replaces an element satisfying the constraint. A composition of `findPath` and `insert`
replace :: forall x c d n.
  ( SymbolicOutput x
  , Context x ~ c
  , SymbolicFold c
  , KnownNat d
  , 1 <= d
  , KnownRegisters c d Auto
  , NumberOfBits (BaseField c) ~ n
  , Zip (Layout x)
  ) => MorphFrom c x (Bool c) -> MerkleTree d x -> x -> MerkleTree d x
replace p t = withNumberOfRegisters @n @Auto @(BaseField c) $ withDict (minusNat @d @1) $
  insertLeaf t (fromMaybe (P.error "That Leaf does not exist") $ findPath @x @c p t)

-- | Returns the next path in a tree
incrementPath :: forall c d.
  ( KnownNat d
  , Symbolic c
  ) => MerkleTreePath d c -> MerkleTreePath d c
incrementPath (MerkleTreePath p) =
  MerkleTreePath . P.fmap Bool . indToPath @c $ fromFieldElement (FieldElement (ind $ P.fmap (\(Bool b) -> b) p) + one)
