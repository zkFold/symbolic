{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Use record patterns" -}

module ZkFold.ArithmeticCircuit.Experimental where

import Control.Applicative (pure, (<*>))
import Control.DeepSeq (NFData (..), rwhnf)
import Control.Monad (unless, (>>=))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState, StateT, runState, runStateT, state)
import Data.Binary (Binary)
import qualified Data.Eq as Prelude
import Data.Function (const, flip, ($), (.))
import Data.Functor (fmap, (<$>))
import Data.HashTable.IO (BasicHashTable)
import qualified Data.HashTable.IO as T
import Data.Kind (Type)
import Data.Maybe (Maybe (..), isJust)
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Traversable (Traversable, traverse)
import Data.Type.Equality (type (~))
import GHC.Err (error)
import GHC.Generics (U1, (:*:) (..))
import GHC.Integer (Integer)
import GHC.TypeNats (KnownNat)
import Numeric.Natural (Natural)
import System.IO (IO)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, makeStableName)

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (Prime)
import ZkFold.Algebra.Polynomial.Multivariate.Lists (Polynomial, evalPoly)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit, optimize, solder)
import ZkFold.ArithmeticCircuit.Context (CircuitContext, crown, emptyContext)
import ZkFold.ArithmeticCircuit.Op
import ZkFold.ArithmeticCircuit.Var (NewVar (..), Var)
import ZkFold.ArithmeticCircuit.Witness (BooleanF, EuclideanF, OrderingF, WitnessF)
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Bool (BoolType (..))
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.Ord (IsOrdering (..), Ord (..))
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Compat (CompatContext (..))
import qualified ZkFold.Symbolic.Compiler as Old
import qualified ZkFold.Symbolic.Data.Class as Old
import ZkFold.Symbolic.Data.V2 (HasRep, Layout, SymbolicData (fromLayout, toLayout))
import ZkFold.Symbolic.MonadCircuit (at, constraint, lookupConstraint, unconstrained)
import ZkFold.Symbolic.V2 (Constraint (..), Symbolic (..))

------------------- Experimental single-output circuit type --------------------

data Node p (s :: Sort) where
  NodeInput :: NewVar -> Node p ZZp
  NodeApply :: KnownSort s => Op (Node p) s -> Node p s
  NodeConstrain :: Constraint (Node p ZZp) -> Node p ZZp -> Node p ZZp

instance NFData (Node p s) where
  rnf = rwhnf -- GADTs are strict, so no need to eval

instance KnownSort s => Conditional (Node p BB) (Node p s) where
  bool onFalse onTrue condition = NodeApply (OpBool onFalse onTrue condition)

instance BoolType (Node p BB) where
  true = one
  false = zero
  not = negate
  (&&) = (*)
  xor = (+)
  x || y = NodeApply (OpOr x y)

instance Semigroup (Node p OO) where
  x <> y = NodeApply (OpAppend x y)

instance Monoid (Node p OO) where
  mempty = zero

instance IsOrdering (Node p OO) where
  lt = fromConstant ((-1) :: Integer)
  eq = zero
  gt = one

instance Eq (Node p s) where
  type BooleanOf (Node p s) = Node p BB
  x == y = NodeApply (OpEq x y)
  x /= y = NodeApply (OpNEq x y)

instance Ord (Node p ZZ) where
  type OrderingOf (Node p ZZ) = Node p OO
  compare x y = NodeApply (OpCompare x y)
  ordering x y z o = NodeApply (OpOrder x y z o)
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
  fromConstant x = NodeApply $ OpConst (fromConstant x)

instance KnownSort s => FromConstant Integer (Node p s) where
  fromConstant x = NodeApply (OpConst x)

instance FromConstant (Node p ZZ) (Node p ZZp) where
  fromConstant x = NodeApply (OpFrom x)

instance KnownSort s => Scale Natural (Node p s) where
  scale k = NodeApply . OpScale (fromConstant k)

instance KnownSort s => Scale Integer (Node p s) where
  scale k x = NodeApply (OpScale k x)

instance KnownSort s => Exponent (Node p s) Natural where
  x ^ p = NodeApply (OpExp x p)

instance Prime p => Exponent (Node p ZZp) Integer where
  (^) = intPowF

instance KnownSort s => Zero (Node p s) where
  zero = fromConstant (0 :: Integer)

instance KnownSort s => AdditiveSemigroup (Node p s) where
  x + y = NodeApply (OpAdd x y)

instance KnownSort s => AdditiveMonoid (Node p s)

instance KnownSort s => AdditiveGroup (Node p s) where
  negate = NodeApply . OpNeg

instance KnownSort s => MultiplicativeSemigroup (Node p s) where
  x * y = NodeApply (OpMul x y)

instance KnownSort s => MultiplicativeMonoid (Node p s) where
  one = fromConstant (1 :: Integer)

instance KnownSort s => Semiring (Node p s)

instance KnownSort s => Ring (Node p s)

instance SemiEuclidean (Node p ZZ) where
  div x y = NodeApply (OpDiv x y)
  mod x y = NodeApply (OpMod x y)

instance Euclidean (Node p ZZ) where
  gcd x y = NodeApply (OpGcd x y)
  bezoutL x y = NodeApply (OpBezoutL x y)
  bezoutR x y = NodeApply (OpBezoutR x y)

instance Prime p => Field (Node p ZZp) where
  finv = NodeApply . OpInv

instance (Prime p, KnownNat (NumberOfBits (Node p ZZp))) => PrimeField (Node p ZZp) where
  type IntegralOf (Node p ZZp) = Node p ZZ
  toIntegral = NodeApply . OpTo

instance (Prime p, KnownNat (NumberOfBits (Node p ZZp))) => Symbolic (Node p ZZp) where
  constrain = NodeConstrain

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
    let (output, circuit) = unsafePerformIO do
          compiler <- makeCompiler
          flip runStateT emptyContext
            . flip runReaderT compiler
            . traverse compileNode
            . compatContext
            . Old.arithmetize
            . Old.apply f
            $ Old.restore (CompatContext (NodeInput <$> l), NodeInput <$> p)
     in crown circuit (toVar <$> output)

compileV2
  :: forall a c f
   . (Arithmetic a, Binary a, c ~ Node (Order a) ZZp, SymbolicFunction c f)
  => f -> ArithmeticCircuit a (Layout (Input f) c) (Layout (Output f) c)
compileV2 =
  optimize . solder . \(f :: f) (l :: Layout (Input f) c NewVar) ->
    let (output, circuit) = unsafePerformIO do
          compiler <- makeCompiler
          flip runStateT emptyContext
            . flip runReaderT compiler
            . traverse compileNode
            . toLayout
            . symApply f
            $ fromLayout (fmap NodeInput l)
     in crown circuit (toVar <$> output)

------------------------- Compilation internals --------------------------------

type StableTable k v = BasicHashTable (StableName k) v

data Compiler a = Compiler
  { constraintLog :: StableTable (Constraint (Node (Order a) ZZp)) ()
  , witnessExtractor :: WitnessExtractor a
  }

type CompilerM a = ReaderT (Compiler a) (StateT (CircuitContext a U1) IO)

makeCompiler :: IO (Compiler a)
makeCompiler = Compiler <$> T.new <*> makeExtractor

compileNode
  :: forall a s
   . (Arithmetic a, Binary a)
  => Node (Order a) s -> CompilerM a (Witness a s)
compileNode (NodeInput v) = pure $ FieldVar (pure v)
compileNode (NodeConstrain !c n) = do
  snc <- liftIO (makeStableName c)
  isDone <-
    asks constraintLog
      >>= liftIO . \log -> T.mutate log snc \x ->
        (Just (), isJust x)
  unless isDone case c of
    Lookup lkp ns -> do
      vs <- traverse (fmap toVar . compileNode) ns
      state $ runState (lookupConstraint vs lkp)
    Polynomial p -> do
      poly <- traverse (fmap toVar . compileNode) p
      state . runState $ constraint (evalPoly @a poly)
  compileNode n
compileNode (NodeApply !op) = do
  sno <- liftIO (makeStableName op)
  asks witnessExtractor >>= liftIO . request sno >>= \case
    Just w -> pure w
    Nothing -> do
      w <- traverseOp compileNode op >>= opToWitness
      asks witnessExtractor >>= liftIO . insertWitness sno w
      pure w

data WitnessExtractor a = WitnessExtractor
  { weVars :: StableTable (Op (Node (Order a)) ZZp) (Var a)
  , weInts :: StableTable (Op (Node (Order a)) ZZ) (EuclideanF a NewVar)
  , weBool :: StableTable (Op (Node (Order a)) BB) (BooleanF a NewVar)
  , weOrds :: StableTable (Op (Node (Order a)) OO) (OrderingF a NewVar)
  }

makeExtractor :: IO (WitnessExtractor a)
makeExtractor = WitnessExtractor <$> T.new <*> T.new <*> T.new <*> T.new

insertWitness
  :: StableName (Op (Node (Order a)) s)
  -> Witness a s
  -> WitnessExtractor a
  -> IO ()
insertWitness sn witness WitnessExtractor {..} = case witness of
  FieldVar v -> T.insert weVars sn v
  IntWitness w -> T.insert weInts sn w
  BoolWitness w -> T.insert weBool sn w
  OrdWitness w -> T.insert weOrds sn w

request
  :: forall a s
   . KnownSort s
  => StableName (Op (Node (Order a)) s)
  -> WitnessExtractor a
  -> IO (Maybe (Witness a s))
request sn WitnessExtractor {..} = case knownSort @s of
  ZZpSing -> fmap FieldVar <$> T.lookup weVars sn
  ZZSing -> fmap IntWitness <$> T.lookup weInts sn
  BBSing -> fmap BoolWitness <$> T.lookup weBool sn
  OOSing -> fmap OrdWitness <$> T.lookup weOrds sn

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
  :: forall a s m
   . (Arithmetic a, Binary a, MonadState (CircuitContext a U1) m)
  => Op (Witness a) s -> m (Witness a s)
opToWitness = \case
  OpConst c -> pure (fromConstant c)
  OpScale k w -> pure (scale k w)
  OpAdd (FieldVar u) (FieldVar v) ->
    state . runState $ FieldVar <$> unconstrained (at u + at v)
  OpAdd (IntWitness v) (IntWitness w) -> pure $ IntWitness (v + w)
  OpAdd (BoolWitness v) (BoolWitness w) -> pure $ BoolWitness (v `xor` w)
  OpAdd (OrdWitness v) (OrdWitness w) ->
    pure $ OrdWitness (v <> w) -- TODO wrong but unused
  OpMul (FieldVar u) (FieldVar v) ->
    state . runState $ FieldVar <$> unconstrained (at u * at v)
  OpMul (IntWitness v) (IntWitness w) -> pure $ IntWitness (v * w)
  OpMul (BoolWitness v) (BoolWitness w) -> pure $ BoolWitness (v && w)
  OpMul (OrdWitness v) (OrdWitness w) ->
    pure $ OrdWitness (v <> w) -- TODO wrong but unused
  OpNeg w -> pure (scale (-1 :: Integer) w)
  OpExp (FieldVar v) p ->
    state . runState $ FieldVar <$> unconstrained (at v ^ p)
  OpExp (IntWitness w) p -> pure $ IntWitness (w ^ p)
  OpExp (BoolWitness w) p -> pure $ BoolWitness (fromConstant (p == 0) || w)
  OpExp (OrdWitness w) _ -> pure (OrdWitness w) -- TODO wrong but unused
  OpFrom (IntWitness w) ->
    state . runState $ FieldVar <$> unconstrained (fromConstant w)
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
    state . runState $ FieldVar <$> unconstrained (at v)
  OpEq x y -> pure $ BoolWitness (x == y)
  OpNEq x y -> pure $ BoolWitness (x /= y)
  OpOr (BoolWitness v) (BoolWitness w) -> pure $ BoolWitness (v || w)
  OpBool (FieldVar u) (FieldVar v) (BoolWitness w) ->
    state . runState $ FieldVar <$> unconstrained (bool (at u) (at v) w)
  OpBool (IntWitness v) (IntWitness w) (BoolWitness b) ->
    pure $ IntWitness (bool v w b)
  OpBool (BoolWitness v) (BoolWitness w) (BoolWitness b) ->
    pure $ BoolWitness (bool v w b)
  OpBool (OrdWitness _) (OrdWitness _) (BoolWitness _) ->
    pure $ OrdWitness (error "not implemented")
  OpAppend (OrdWitness v) (OrdWitness w) -> pure $ OrdWitness (v <> w)
  OpOrder (IntWitness u) (IntWitness v) (IntWitness w) (OrdWitness o) ->
    pure $ IntWitness (ordering u v w o)

instance {-# OVERLAPPING #-} (Ring a, Prelude.Eq a) => Scale v (Polynomial a v)

instance {-# OVERLAPPING #-} (Ring a, Prelude.Eq a) => FromConstant v (Polynomial a v) where
  fromConstant = pure
