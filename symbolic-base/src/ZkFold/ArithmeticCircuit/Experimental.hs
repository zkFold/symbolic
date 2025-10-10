{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Use record patterns" -}

module ZkFold.ArithmeticCircuit.Experimental where

import Control.Applicative (pure)
import Control.DeepSeq (NFData (..), rwhnf)
import Control.Monad (unless, (>>=))
import Control.Monad.State (State, gets, modify', runState)
import Crypto.Hash.SHA256 (hash)
import Data.Bifunctor (first)
import Data.Binary (Binary)
import Data.Bool (Bool (..))
import Data.ByteString (ByteString)
import qualified Data.Eq as Prelude
import Data.Foldable (foldMap)
import Data.Function (const, flip, ($), (.))
import Data.Functor (Functor, fmap, (<$>))
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..))
import qualified Data.Ord as Prelude
import Data.Semigroup (Semigroup (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (Traversable, traverse)
import Data.Type.Equality (type (~))
import GHC.Err (error)
import GHC.Generics (Generic, Par1 (..), U1, (:*:) (..))
import GHC.Integer (Integer)
import GHC.IsList (fromList, toList)
import GHC.TypeNats (KnownNat)
import Numeric.Natural (Natural)
import Optics (zoom, over)

import ZkFold.Algebra.Class
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number (Prime)
import ZkFold.Algebra.Polynomial.Multivariate.Maps (Polynomial, evalPoly, traversePoly)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit, optimize, solder)
import ZkFold.ArithmeticCircuit.Context (CircuitContext, crown, emptyContext)
import ZkFold.ArithmeticCircuit.Op
import ZkFold.ArithmeticCircuit.Var (NewVar (..), Var)
import ZkFold.ArithmeticCircuit.Witness (BooleanF, EuclideanF, OrderingF, WitnessF)
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Binary (toByteString)
import ZkFold.Data.Bool (BoolType (..))
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.Ord (IsOrdering (..), Ord (..))
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Compat (CompatContext (..))
import qualified ZkFold.Symbolic.Compiler as Old
import qualified ZkFold.Symbolic.Data.Class as Old
import ZkFold.Symbolic.Data.V2 (HasRep, Layout, SymbolicData (fromLayout, toLayout))
import ZkFold.Symbolic.MonadCircuit (at, constraint, lookupConstraint, unconstrained)
import ZkFold.Symbolic.V2 (Constraint (..), LookupTable (..), Symbolic (..))

------------------- Experimental single-output circuit type --------------------

type Hash = ByteString

data Node p (s :: Sort) where
  NodeInput :: Hash -> Node p ZZp
  NodeApply :: KnownSort s => Op (Node p) s -> Hash -> Node p s
  NodeConstrain :: Constraint (Node p ZZp) -> Node p ZZp -> Hash -> Node p ZZp

fromNewVar :: NewVar -> Node n ZZp
fromNewVar (EqVar v) = NodeInput v
fromNewVar _ = error "folding not supported"

instance Prelude.Eq (Node p s) where
  NodeInput h == NodeInput h' = h Prelude.== h'
  NodeApply _ h == NodeApply _ h' = h Prelude.== h'
  NodeConstrain _ _ h == NodeConstrain _ _ h' = h Prelude.== h'
  _ == _ = False

instance Prelude.Ord (Node p s) where
  NodeInput h `compare` NodeInput h' = h `Prelude.compare` h'
  NodeInput _ `compare` _ = Prelude.LT
  NodeApply _ _ `compare` NodeInput _ = Prelude.GT
  NodeApply _ h `compare` NodeApply _ h' = h `Prelude.compare` h'
  NodeApply _ _ `compare` NodeConstrain _ _ _ = Prelude.LT
  NodeConstrain _ _ h `compare` NodeConstrain _ _ h' = h `Prelude.compare` h'
  NodeConstrain _ _ _ `compare` _ = Prelude.GT

instance NFData (Node p s) where
  rnf = rwhnf -- GADTs are strict, so no need to eval

apply :: KnownSort s => Op (Node p) s -> Node p s
apply op = NodeApply op $ hash $ opToBinary getHash op

getHash :: Node p s -> Hash
getHash = \case
  NodeInput h -> h
  NodeApply _ h -> h
  NodeConstrain _ _ h -> h

instance KnownSort s => Conditional (Node p BB) (Node p s) where
  bool onFalse onTrue condition = apply (OpBool onFalse onTrue condition)

instance BoolType (Node p BB) where
  true = one
  false = zero
  not = negate
  (&&) = (*)
  xor = (+)
  x || y = apply (OpOr x y)

instance Semigroup (Node p OO) where
  x <> y = apply (OpAppend x y)

instance Monoid (Node p OO) where
  mempty = zero

instance IsOrdering (Node p OO) where
  lt = fromConstant ((-1) :: Integer)
  eq = zero
  gt = one

instance Eq (Node p s) where
  type BooleanOf (Node p s) = Node p BB
  x == y = apply (OpEq x y)
  x /= y = apply (OpNEq x y)

instance Ord (Node p ZZ) where
  type OrderingOf (Node p ZZ) = Node p OO
  compare x y = apply (OpCompare x y)
  ordering x y z o = apply (OpOrder x y z o)
  x < y = compare x y == lt
  x <= y = compare x y /= gt
  x >= y = compare x y /= lt
  x > y = compare x y == gt

instance
  (KnownNat p, KnownNat (NumberOfBits (Node p ZZp)))
  => Finite (Node p ZZp)
  where
  type Order (Node p ZZp) = p

instance KnownSort s => FromConstant Natural (Node p s) where
  fromConstant x = apply $ OpConst (fromConstant x)

instance KnownSort s => FromConstant Integer (Node p s) where
  fromConstant x = apply (OpConst x)

instance FromConstant (Node p ZZ) (Node p ZZp) where
  fromConstant x = apply (OpFrom x)

instance KnownSort s => Scale Natural (Node p s) where
  scale k = apply . OpScale (fromConstant k)

instance KnownSort s => Scale Integer (Node p s) where
  scale k x = apply (OpScale k x)

instance KnownSort s => Exponent (Node p s) Natural where
  x ^ p = apply (OpExp x p)

instance Prime p => Exponent (Node p ZZp) Integer where
  (^) = intPowF

instance KnownSort s => Zero (Node p s) where
  zero = fromConstant (0 :: Integer)

instance KnownSort s => AdditiveSemigroup (Node p s) where
  x + y = apply (OpAdd x y)

instance KnownSort s => AdditiveMonoid (Node p s)

instance KnownSort s => AdditiveGroup (Node p s) where
  negate x = apply (OpNeg x)

instance KnownSort s => MultiplicativeSemigroup (Node p s) where
  x * y = apply (OpMul x y)

instance KnownSort s => MultiplicativeMonoid (Node p s) where
  one = fromConstant (1 :: Integer)

instance KnownSort s => Semiring (Node p s)

instance KnownSort s => Ring (Node p s)

instance SemiEuclidean (Node p ZZ) where
  div x y = apply (OpDiv x y)
  mod x y = apply (OpMod x y)

instance Euclidean (Node p ZZ) where
  gcd x y = apply (OpGcd x y)
  bezoutL x y = apply (OpBezoutL x y)
  bezoutR x y = apply (OpBezoutR x y)

instance Prime p => Field (Node p ZZp) where
  finv x = apply (OpInv x)

instance (Prime p, KnownNat (NumberOfBits (Node p ZZp))) => PrimeField (Node p ZZp) where
  type IntegralOf (Node p ZZp) = Node p ZZ
  toIntegral x = apply (OpTo x)

instance (Prime p, KnownNat (NumberOfBits (Node p ZZp))) => Symbolic (Node p ZZp) where
  constrain c x = NodeConstrain c x $ hash (constrToByteString c <> getHash x)
   where
    constrToByteString (Polynomial p) =
      toByteString $
        first (first getHash <$>)
          <$> toList @(Polynomial (Zp p) (Node p ZZp)) p
    constrToByteString (Lookup tb xs) = lookupToByteString tb xs
    lookupToByteString :: LookupTable f -> f (Node p ZZp) -> ByteString
    lookupToByteString (Ranges rs) (Par1 i) =
      foldMap toByteString rs <> getHash i
    lookupToByteString (Product t u) (i :*: j) =
      lookupToByteString t i <> lookupToByteString u j
    lookupToByteString (Plot f t) (i :*: j) =
      lookupToByteString t i <> foldMap getHash j <> foldMap getHash (f i)

------------------------- Optimized compilation function -----------------------

type family InputF (f :: Type) where
  InputF (i a -> f) = i :*: Input f
  InputF (o a) = U1

type family OutputF (f :: Type) where
  OutputF (i a -> f) = Output f
  OutputF (o a) = o

class
  ( SymbolicData (Input f)
  , HasRep (Input f) a
  , SymbolicData (Output f)
  , Traversable (Layout (Output f) a)
  ) =>
  SymbolicFunction (a :: Type) (f :: Type)
    | f -> a
  where
  type Input f :: Type -> Type
  type Input f = InputF f
  type Output f :: Type -> Type
  type Output f = OutputF f
  symApply :: f -> Input f a -> Output f a

instance
  (SymbolicData o, Traversable (Layout o a), Input (o a) ~ U1, Output (o a) ~ o)
  => SymbolicFunction a (o a)
  where
  symApply = const

instance
  (SymbolicData i, HasRep i a, SymbolicFunction a f)
  => SymbolicFunction a (i a -> f)
  where
  symApply f (x :*: y) = symApply (f x) y

compileV1
  :: forall a f n d
   . ( Arithmetic a
     , Binary a
     , Old.SymbolicFunction f
     , Order a ~ n
     , Old.Context f ~ CompatContext (Node n ZZp)
     , Old.Domain f ~ d
     )
  => f
  -> ArithmeticCircuit
       a
       (Old.Layout d n :*: Old.Payload d n)
       (Old.Layout (Old.Range f) n)
compileV1 =
  optimize . solder . \f (l :*: p) ->
    let (output, compiler) =
          flip runState makeCompiler
            . traverse compileNode
            . compatContext
            . Old.arithmetize
            . Old.apply f
            $ Old.restore (CompatContext (fromNewVar <$> l), fromNewVar <$> p)
     in compileOutput compiler output

compileV2
  :: forall a c f
   . (Arithmetic a, Binary a, c ~ Node (Order a) ZZp, SymbolicFunction c f)
  => f -> ArithmeticCircuit a (Layout (Input f) c) (Layout (Output f) c)
compileV2 =
  optimize . solder . \(f :: f) (l :: Layout (Input f) c NewVar) ->
    let (output, compiler) =
          flip runState makeCompiler
            . traverse compileNode
            . toLayout
            . symApply f
            $ fromLayout (fmap fromNewVar l)
     in compileOutput compiler output

------------------------- Compilation internals --------------------------------

data Compiler a = Compiler
  { circuitContext :: CircuitContext a U1
  , constraintLog :: Set Hash
  , witnessExtractor :: WitnessExtractor a
  }
  deriving Generic

makeCompiler :: Compiler a
makeCompiler = Compiler emptyContext S.empty makeExtractor

compileNode
  :: forall a s
   . (Arithmetic a, Binary a)
  => Node (Order a) s -> State (Compiler a) (Witness a s)
compileNode (NodeInput v) = pure $ FieldVar $ pure (EqVar v)
compileNode (NodeConstrain c n h) = do
  isDone <- gets (S.member h . constraintLog)
  zoom #constraintLog $ modify' (S.insert h)
  unless isDone case c of
    Lookup lkp ns -> do
      vs <- traverse (fmap toVar . compileNode) ns
      zoom #circuitContext (lookupConstraint vs lkp)
    Polynomial p -> do
      poly <- traversePoly @_ @a (fmap toVar . compileNode) p
      zoom #circuitContext $ constraint (evalPoly poly)
  compileNode n
compileNode (NodeApply op h) =
  gets (request h . witnessExtractor) >>= \case
    Just w -> pure w
    Nothing -> do
      w <- traverseOp compileNode op >>= opToWitness
      zoom #witnessExtractor $ modify' $ insertWitness h w
      pure w

compileOutput
  :: Functor o => Compiler a -> o (Witness a ZZp) -> CircuitContext a o
compileOutput Compiler {..} = crown circuitContext . fmap toVar

data WitnessExtractor a = WitnessExtractor
  { weVars :: Map Hash (Var a)
  , weInts :: Map Hash (EuclideanF a NewVar)
  , weBool :: Map Hash (BooleanF a NewVar)
  , weOrds :: Map Hash (OrderingF a NewVar)
  }
  deriving Generic

makeExtractor :: WitnessExtractor a
makeExtractor = WitnessExtractor M.empty M.empty M.empty M.empty

insertWitness :: Hash -> Witness a s -> WitnessExtractor a -> WitnessExtractor a
insertWitness h = \case
  FieldVar v -> over #weVars (M.insert h v)
  IntWitness w -> over #weInts (M.insert h w)
  BoolWitness w -> over #weBool (M.insert h w)
  OrdWitness w -> over #weOrds (M.insert h w)

request
  :: forall a s. KnownSort s
  => Hash -> WitnessExtractor a -> Maybe (Witness a s)
request h WitnessExtractor {..} = case knownSort @s of
  ZZpSing -> FieldVar <$> weVars M.!? h
  ZZSing -> IntWitness <$> weInts M.!? h
  BBSing -> BoolWitness <$> weBool M.!? h
  OOSing -> OrdWitness <$> weOrds M.!? h

data Witness a (s :: Sort) where
  FieldVar :: Var a -> Witness a ZZp
  IntWitness :: EuclideanF a NewVar -> Witness a ZZ
  BoolWitness :: BooleanF a NewVar -> Witness a BB
  OrdWitness :: OrderingF a NewVar -> Witness a OO

toVar :: Witness a ZZp -> Var a
toVar (FieldVar v) = v

instance (KnownSort s, FromConstant Integer a) => FromConstant Integer (Witness a s) where
  fromConstant = case knownSort @s of
    ZZpSing -> FieldVar . fromConstant
    ZZSing -> IntWitness . fromConstant
    BBSing -> BoolWitness . fromConstant
    OOSing -> OrdWitness . fromConstant

instance Scale Integer a => Scale Integer (Witness a s) where
  scale k = \case
    FieldVar v -> FieldVar (scale k v)
    IntWitness w -> IntWitness (scale k w)
    BoolWitness w -> BoolWitness (fromConstant k && w)
    OrdWitness w -> OrdWitness (fromConstant k <> w) -- TODO: wrong but unused

instance PrimeField a => Eq (Witness a s) where
  type BooleanOf (Witness a s) = BooleanF a NewVar
  FieldVar u == FieldVar v = at @_ @(WitnessF a NewVar) u == at v
  IntWitness v == IntWitness w = v == w
  BoolWitness v == BoolWitness w = not (xor v w)
  OrdWitness _ == OrdWitness _ = error "not implemented"
  x /= y = not (x == y)

opToWitness
  :: forall a s
   . (Arithmetic a, Binary a)
  => Op (Witness a) s -> State (Compiler a) (Witness a s)
opToWitness = \case
  OpConst c -> pure (fromConstant c)
  OpScale k w -> pure (scale k w)
  OpAdd (FieldVar u) (FieldVar v) ->
    zoom #circuitContext $ FieldVar <$> unconstrained (at u + at v)
  OpAdd (IntWitness v) (IntWitness w) -> pure $ IntWitness (v + w)
  OpAdd (BoolWitness v) (BoolWitness w) -> pure $ BoolWitness (v `xor` w)
  OpAdd (OrdWitness v) (OrdWitness w) ->
    pure $ OrdWitness (v <> w) -- TODO wrong but unused
  OpMul (FieldVar u) (FieldVar v) ->
    zoom #circuitContext $ FieldVar <$> unconstrained (at u * at v)
  OpMul (IntWitness v) (IntWitness w) -> pure $ IntWitness (v * w)
  OpMul (BoolWitness v) (BoolWitness w) -> pure $ BoolWitness (v && w)
  OpMul (OrdWitness v) (OrdWitness w) ->
    pure $ OrdWitness (v <> w) -- TODO wrong but unused
  OpNeg w -> pure (scale (-1 :: Integer) w)
  OpExp (FieldVar v) p ->
    zoom #circuitContext $ FieldVar <$> unconstrained (at v ^ p)
  OpExp (IntWitness w) p -> pure $ IntWitness (w ^ p)
  OpExp (BoolWitness w) p -> pure $ BoolWitness (fromConstant (p == 0) || w)
  OpExp (OrdWitness w) _ -> pure (OrdWitness w) -- TODO wrong but unused
  OpFrom (IntWitness w) ->
    zoom #circuitContext $ FieldVar <$> unconstrained (fromConstant w)
  OpTo (FieldVar v) ->
    pure $ IntWitness $ toIntegral @(WitnessF a NewVar) (at v)
  OpCompare (IntWitness v) (IntWitness w) ->
    pure $ OrdWitness (v `compare` w)
  OpDiv (IntWitness v) (IntWitness w) -> pure $ IntWitness (v `div` w)
  OpMod (IntWitness v) (IntWitness w) -> pure $ IntWitness (v `mod` w)
  OpGcd (IntWitness v) (IntWitness w) -> pure $ IntWitness (v `gcd` w)
  OpBezoutL (IntWitness v) (IntWitness w) ->
    pure $ IntWitness (v `bezoutL` w)
  OpBezoutR (IntWitness v) (IntWitness w) ->
    pure $ IntWitness (v `bezoutR` w)
  OpInv (FieldVar v) ->
    zoom #circuitContext $ FieldVar <$> unconstrained (at v)
  OpEq x y -> pure $ BoolWitness (x == y)
  OpNEq x y -> pure $ BoolWitness (x /= y)
  OpOr (BoolWitness v) (BoolWitness w) -> pure $ BoolWitness (v || w)
  OpBool (FieldVar u) (FieldVar v) (BoolWitness w) ->
    zoom #circuitContext $ FieldVar <$> unconstrained (bool (at u) (at v) w)
  OpBool (IntWitness v) (IntWitness w) (BoolWitness b) ->
    pure $ IntWitness (bool v w b)
  OpBool (BoolWitness v) (BoolWitness w) (BoolWitness b) ->
    pure $ BoolWitness (bool v w b)
  OpBool (OrdWitness _) (OrdWitness _) (BoolWitness _) ->
    pure $ OrdWitness (error "not implemented")
  OpAppend (OrdWitness v) (OrdWitness w) -> pure $ OrdWitness (v <> w)
  OpOrder (IntWitness u) (IntWitness v) (IntWitness w) (OrdWitness o) ->
    pure $ IntWitness (ordering u v w o)

instance
  {-# OVERLAPPING #-}
  (AdditiveMonoid a, Prelude.Eq a, Prelude.Ord v)
  => Scale v (Polynomial a v)
  where
  scale x = fromList . fmap (first ((x, 1) :)) . toList

instance
  {-# OVERLAPPING #-}
  (Semiring a, Prelude.Eq a, Prelude.Ord v)
  => FromConstant v (Polynomial a v)
  where
  fromConstant x = fromList [([(x, 1)], one)]
