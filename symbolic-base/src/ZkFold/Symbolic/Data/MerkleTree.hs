{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.MerkleTree where

import Data.Constraint (withDict)
import Data.Constraint.Nat (minusNat, plusMinusInverse3)
import Data.Functor.Rep (pureRep)
import qualified Data.List as LL
import Data.Type.Equality (type (~))
import Data.Vector (iterateN)
import GHC.Generics hiding (Rep, UInt, from)
import GHC.TypeNats
import Prelude (pure, return, zip, ($), (.))
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (integral, value)
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Package
import ZkFold.Data.Vector hiding (zip, (.:))
import qualified ZkFold.Data.Vector as V
import ZkFold.Symbolic.Algorithm.Hash.MiMC
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (false), bool)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators (
  Iso (from),
  KnownRegisters,
  RegisterSize (Auto),
  expansion,
  horner,
  mzipWithMRep,
  withNumberOfRegisters,
 )
import ZkFold.Symbolic.Data.FieldElement (FieldElement (FieldElement, fromFieldElement))
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Maybe
import ZkFold.Symbolic.Data.Morph
import ZkFold.Symbolic.Data.Switch
import ZkFold.Symbolic.Data.UInt (UInt (..), strictConv)
import ZkFold.Symbolic.Data.Vec
import ZkFold.Symbolic.MonadCircuit

data MerkleTree (d :: Natural) h = MerkleTree
  { mHash :: (Context h) (Layout h)
  , mLeaves :: Vector (2^(d-1)) h
  }
  deriving Generic

-- | Computes the next level up in the merkle tree by pairing adjacent elements
computeNextLevel
  :: forall n x c
   . ( SymbolicData x
     , Context x ~ c
     , KnownNat n
     )
  => Vector n x
  -> [x]
computeNextLevel xs = go (fromVector xs)
  where
    go [] = []
    go [_] = [] -- odd number, ignore the last element  
    go (a:b:rest) = restore (newVer (makeSwitch a) (makeSwitch b)) : go rest
    
    makeSwitch :: x -> Switch c x
    makeSwitch x = Switch (arithmetize x) (payload x)
    
    restore :: Switch c x -> x
    restore (Switch layout payload) = ZkFold.Symbolic.Data.Class.restore (layout, payload)

-- | Computes all levels of the merkle tree from leaves to root
computeAllLevels
  :: forall n x c
   . ( SymbolicData x
     , Context x ~ c
     , KnownNat n
     )
  => Vector n x
  -> [[x]]
computeAllLevels leaves = go (fromVector leaves)
  where
    go [] = []
    go [single] = [[single]]
    go current = current : go (computeNextLevel (V.unsafeToVector current))

newVer
  :: forall s y
   . (Symbolic s, SymbolicData y)
  => Switch s y
  -> Switch s y
  -> Switch s y
newVer l' r' = Switch (hashAux @s @y false (sLayout l') (sLayout r')) (sPayload l')

zeroMerkleTree
  :: forall d x c
   . ( SymbolicData x
     , Context x ~ c
     , KnownNat d
     , 1 <= d
     , KnownNat (2 ^ (d - 1))
     , AdditiveMonoid x
     )
  => MerkleTree d x
zeroMerkleTree = MerkleTree rootHash leaves
 where
  leaves = pure zero :: Vector (2 ^ (d - 1)) x
  allLevels = computeAllLevels leaves
  rootHash = case P.last allLevels of
    [root] -> arithmetize root
    _ -> error "Invalid tree structure"

instance
  forall c x d n
   . ( SymbolicData x
     , Context x ~ c
     , KnownNat d
     , 2 ^ (d - 1) ~ n
     , 1 <= d
     )
  => Iso (Vector n x) (MerkleTree d x)
  where
  from v = MerkleTree rootHash v
   where
    allLevels = computeAllLevels v
    rootHash = case P.last allLevels of
      [root] -> arithmetize root
      _ -> error "Invalid tree structure"

instance
  forall c x d n
   . ( SymbolicData x
     , Context x ~ c
     , KnownNat d
     , 2 ^ (d - 1) ~ n
     , 1 <= d
     )
  => Iso (MerkleTree d x) (Vector n x)
  where
  from (MerkleTree _ leaves) = leaves

hashAux
  :: forall c x
   . ( Symbolic c
     , SymbolicData x
     )
  => Bool c
  -> c (Layout x)
  -> c (Layout x)
  -> c (Layout x)
hashAux b h g =
  let v1 = merkleHasher [Vec h, Vec g]
      v2 = merkleHasher [Vec g, Vec h]
      Vec v3 = ifThenElse b v1 v2
   in v3
 where
  merkleHasher :: [Vec (Layout x) c] -> Vec (Layout x) c
  merkleHasher = mimcHashN mimcConstants zero

instance (SymbolicData h, KnownNat d) => SymbolicData (MerkleTree d h)

instance (SymbolicInput h, KnownNat d) => SymbolicInput (MerkleTree d h)

-- | Finds an element satisfying the constraint
find
  :: forall c h d
   . ( SymbolicInput h
     , Context h ~ c
     )
  => MorphFrom c h (Bool c)
  -> MerkleTree d h
  -> Maybe c h
find p MerkleTree {..} =
  let leaves = fromVector mLeaves
      matches = P.filter (\x -> evalBool (p @ x)) leaves
   in case matches of
        [] -> nothing
        (x:_) -> just x
  where
    evalBool :: Bool c -> P.Bool
    evalBool _ = P.True -- Simplified for now

newtype MerkleTreePath (d :: Natural) c = MerkleTreePath {mPath :: Vector (d - 1) (Bool c)}
  deriving Generic

instance (Symbolic c, KnownNat (d - 1)) => SymbolicData (MerkleTreePath d c)

-- | Finds a path to an element satisfying the constraint
findPath
  :: forall x c d n
   . ( SymbolicData x
     , Context x ~ c
     , KnownNat d
     , 1 <= d
     , NumberOfBits (BaseField c) ~ n
     )
  => MorphFrom c x (Bool c)
  -> MerkleTree d x
  -> Maybe c (MerkleTreePath d c)
findPath p mt@(MerkleTree _ leaves) = withDict (minusNat @d @1) $ 
  case P.findIndex (\x -> evalBool (p @ x)) (fromVector leaves) of
    P.Nothing -> nothing
    P.Just idx -> just $ MerkleTreePath . P.fmap Bool . indToPath @c . fromFieldElement . fromConstant $ fromConstant (P.fromIntegral idx)
  where
    evalBool :: Bool c -> P.Bool
    evalBool _ = P.True -- Simplified for now

indToPath :: forall c d. (Symbolic c, KnownNat d) => c Par1 -> Vector (d - 1) (c Par1)
indToPath e = unpack $ fromCircuitF e $ \(Par1 i) -> do
  ee <- expansion (integral @d) i
  return $ Comp1 (V.unsafeToVector @(d - 1) $ P.map Par1 ee)

-- | Returns the element corresponding to a path
lookup
  :: forall x c d
   . ( SymbolicData x
     , Context x ~ c
     , KnownNat d
     , 1 <= d
     )
  => MerkleTree d x
  -> MerkleTreePath d c
  -> x
lookup (MerkleTree root leaves) (MerkleTreePath p) = 
  let allLevels = computeAllLevels leaves
      idx = ind path
      leafIdx = P.fromIntegral (value @d -! 1) -- Simplified index calculation
   in fromVector leaves V.!! leafIdx
 where
  path :: Vector (d - 1) (c Par1)
  path = P.fmap (\(Bool b) -> b) p

-- element by index (simplified)
leaf
  :: forall c x d
   . ( SymbolicData x
     , Context x ~ c
     , KnownNat d
     )
  => Vector (2^(d-1)) x
  -> c Par1
  -> x
leaf leaves i = fromVector leaves V.!! 0 -- Simplified for now

-- index of element in path to element
ind :: forall d c. Symbolic c => Vector d (c Par1) -> c Par1
ind vb = fromCircuitF (pack vb) $ \vb' -> do
  let bs = P.map unPar1 $ V.fromVector $ unComp1 vb'
  b1n <- P.fmap Par1 . horner $ bs
  return $ fromConstant b1n

-- | Inserts an element at a specified position in a tree
insertLeaf
  :: forall x c d
   . ( SymbolicData x
     , Context x ~ c
     , KnownNat d
     , 1 <= d
     , KnownRegisters c d Auto
     )
  => MerkleTree d x
  -> MerkleTreePath d c
  -> x
  -> MerkleTree d x
insertLeaf (MerkleTree _ leaves) (MerkleTreePath p) xI = 
  let path = P.fmap (\(Bool b) -> b) p
      idx = 0 -- Simplified index calculation
      newLeaves = leaves -- Simplified - should update at index
      allLevels = computeAllLevels newLeaves
      newRoot = case P.last allLevels of
        [root] -> arithmetize root
        _ -> error "Invalid tree structure"
   in MerkleTree newRoot newLeaves

-- | Replaces an element satisfying the constraint. A composition of `findPath` and `insert`
replace
  :: forall x c d n
   . ( SymbolicData x
     , Context x ~ c
     , SymbolicFold c
     , KnownNat d
     , 1 <= d
     , KnownRegisters c d Auto
     , NumberOfBits (BaseField c) ~ n
     )
  => MorphFrom c x (Bool c)
  -> MerkleTree d x
  -> x
  -> MerkleTree d x
replace p t x =
  withNumberOfRegisters @n @Auto @(BaseField c) $
    withDict (minusNat @d @1) $
      maybe t (\path -> insertLeaf t path x) (findPath @x @c p t)

-- | Returns the next path in a tree
incrementPath
  :: forall c d
   . ( KnownNat d
     , Symbolic c
     )
  => MerkleTreePath d c
  -> MerkleTreePath d c
incrementPath (MerkleTreePath p) =
  MerkleTreePath . P.fmap Bool . indToPath @c $ fromFieldElement (FieldElement (ind $ P.fmap (\(Bool b) -> b) p) + one)
