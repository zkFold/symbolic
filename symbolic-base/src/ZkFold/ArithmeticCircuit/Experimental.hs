{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Use record patterns" -}

module ZkFold.ArithmeticCircuit.Experimental where

import Control.Applicative (Applicative, pure, (<*>))
import Control.DeepSeq (NFData (..), rwhnf)
import Control.Monad (unless, (>>=))
import Control.Monad.State (State, gets, modify', runState)
import Data.Bifunctor (first)
import Data.Binary (Binary)
import Data.Bool (Bool (..))
import Data.ByteString (ByteString)
import qualified Data.Eq as Prelude
import Data.Function (const, flip, ($), (.))
import Data.Functor (Functor, fmap, (<$>), (<&>))
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
import GHC.Generics (Generic, U1, (:*:) (..))
import GHC.Integer (Integer)
import GHC.IsList (fromList, toList)
import GHC.TypeNats (KnownNat)
import Numeric.Natural (Natural)
import Optics (zoom)
import Unsafe.Coerce (unsafeCoerce)

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (Prime)
import ZkFold.Algebra.Polynomial.Multivariate.Maps (Polynomial, evalPoly, traversePoly)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit, optimize, solder)
import ZkFold.ArithmeticCircuit.Context (CircuitContext, crown, emptyContext)
import ZkFold.ArithmeticCircuit.Var (NewVar (..), Var)
import ZkFold.ArithmeticCircuit.Witness (BooleanF, EuclideanF, OrderingF)
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Bool (BoolType (..))
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.Ord (IsOrdering (..), Ord (..))
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Compiler ()
import ZkFold.Symbolic.Data.V2 (HasRep, Layout, SymbolicData (fromLayout, toLayout))
import ZkFold.Symbolic.MonadCircuit (MonadCircuit (constraint, lookupConstraint))
import ZkFold.Symbolic.V2 (Constraint (..), Symbolic (..))

------------------- Experimental single-output circuit type --------------------

type Hash = ByteString

data Sort = ZZp | ZZ | BB | Ordering

data Op f (s :: Sort) where
  OpConst :: Integer -> Op f s
  OpScale :: Integer -> f s -> Op f s
  OpAdd, OpMul :: f s -> f s -> Op f s
  OpNeg :: f s -> Op f s
  OpExp :: f s -> Natural -> Op f s
  OpFrom :: f ZZ -> Op f ZZp
  OpTo :: f ZZp -> Op f ZZ
  OpCompare :: f ZZ -> f ZZ -> Op f Ordering
  OpDiv, OpMod, OpGcd, OpBezoutL, OpBezoutR :: f ZZ -> f ZZ -> Op f ZZ
  OpInv :: f ZZp -> Op f ZZp
  OpEq, OpNEq :: f s -> f s -> Op f BB
  OpOr :: f BB -> f BB -> Op f BB
  OpBool :: f s -> f s -> f BB -> Op f s
  OpOrder :: f s -> f s -> f s -> f Ordering -> Op f s

traverseOp
  :: Applicative m => (forall t. f t -> m (g t)) -> Op f s -> m (Op g s)
traverseOp f = \case
  OpConst i -> pure (OpConst i)
  OpScale i x -> OpScale i <$> f x
  OpAdd x y -> OpAdd <$> f x <*> f y
  OpMul x y -> OpMul <$> f x <*> f y
  OpNeg x -> OpNeg <$> f x
  OpExp x e -> (`OpExp` e) <$> f x
  OpFrom x -> OpFrom <$> f x
  OpTo x -> OpTo <$> f x
  OpCompare x y -> OpCompare <$> f x <*> f y
  OpDiv x y -> OpDiv <$> f x <*> f y
  OpMod x y -> OpMod <$> f x <*> f y
  OpGcd x y -> OpGcd <$> f x <*> f y
  OpBezoutL x y -> OpBezoutL <$> f x <*> f y
  OpBezoutR x y -> OpBezoutR <$> f x <*> f y
  OpInv x -> OpInv <$> f x
  OpEq x y -> OpEq <$> f x <*> f y
  OpNEq x y -> OpNEq <$> f x <*> f y
  OpOr x y -> OpOr <$> f x <*> f y
  OpBool x y z -> OpBool <$> f x <*> f y <*> f z
  OpOrder x y z w -> OpOrder <$> f x <*> f y <*> f z <*> f w

data Node p (s :: Sort) where
  NodeInput :: Hash -> Node p ZZp
  NodeApply :: Op (Node p) s -> Hash -> Node p s
  NodeConstrain :: Constraint (Node p ZZp) -> Node p ZZp -> Hash -> Node p ZZp

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

apply :: Op (Node p) s -> Node p s
apply op = NodeApply op (error "TODO")

instance Conditional (Node p BB) (Node p s) where
  bool onFalse onTrue condition = apply (OpBool onFalse onTrue condition)

instance BoolType (Node p BB) where
  true = one
  false = zero
  not = negate
  (&&) = (*)
  xor = (+)
  x || y = apply (OpOr x y)

instance Semigroup (Node p Ordering) where
  x <> y = apply (OpOrder x y x x)

instance Monoid (Node p Ordering) where
  mempty = zero

instance IsOrdering (Node p Ordering) where
  lt = fromConstant ((-1) :: Integer)
  eq = zero
  gt = one

instance Eq (Node p s) where
  type BooleanOf (Node p s) = Node p BB
  x == y = apply (OpEq x y)
  x /= y = apply (OpNEq x y)

instance Ord (Node p ZZ) where
  type OrderingOf (Node p ZZ) = Node p Ordering
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

instance FromConstant Natural (Node p s) where
  fromConstant x = apply $ OpConst (fromConstant x)

instance FromConstant Integer (Node p s) where
  fromConstant x = apply (OpConst x)

instance FromConstant (Node p ZZ) (Node p ZZp) where
  fromConstant x = apply (OpFrom x)

instance Scale Natural (Node p s) where
  scale k x = apply (OpScale (fromConstant k) x)

instance Scale Integer (Node p s) where
  scale k x = apply (OpScale k x)

instance Exponent (Node p s) Natural where
  x ^ p = apply (OpExp x p)

instance Prime p => Exponent (Node p ZZp) Integer where
  (^) = intPowF

instance Zero (Node p s) where
  zero = apply (OpConst 0)

instance AdditiveSemigroup (Node p s) where
  x + y = apply (OpAdd x y)

instance AdditiveMonoid (Node p s)

instance AdditiveGroup (Node p s) where
  negate x = apply (OpNeg x)

instance MultiplicativeSemigroup (Node p s) where
  x * y = apply (OpMul x y)

instance MultiplicativeMonoid (Node p s) where
  one = apply (OpConst 1)

instance Semiring (Node p s)

instance Ring (Node p s)

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
  constrain c x = NodeConstrain c x (error "TODO")

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

data Witness a s where
  FieldVar :: Var a -> Witness a ZZp
  IntWitness :: EuclideanF a (Var a) -> Witness a ZZ
  BoolWitness :: BooleanF a (Var a) -> Witness a BB
  OrdWitness :: OrderingF a (Var a) -> Witness a Ordering

toVar :: Witness a ZZp -> Var a
toVar (FieldVar v) = v

data SomeWitness a = forall s. SomeWitness (Witness a s)

data Compiler a = Compiler
  { circuitContext :: CircuitContext a U1
  , constraintLog :: Set Hash
  , witnessExtractor :: Map Hash (SomeWitness a)
  }
  deriving Generic

makeCompiler :: Compiler a
makeCompiler = Compiler emptyContext S.empty M.empty

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
  gets (request op . witnessExtractor) >>= \case
    Just w -> pure w
    Nothing -> do
      w <-
        traverseOp compileNode op >>= \case
          OpConst c -> _
      zoom #witnessExtractor $ modify' $ M.insert h (SomeWitness w)
      pure w
 where
  request :: Op f t -> Map Hash (SomeWitness a) -> Maybe (Witness a t)
  request _ m =
    (m M.!? h) <&> \case
      SomeWitness s -> unsafeCoerce s

-- \^ safe assuming no collisions

--    (Just w, EqVar bs) -> do
--      isDone <- gets (M.member bs . acWitness)
--      unless isDone do
--        zoom #acWitness $ modify' $ M.insert bs (fmap elHash w)
--        let MkCBox {..} = elConstraints el
--        for_ cbPolyCon \c -> do
--          let asWitness = WitnessF @a (runPolynomial c)
--              cId = witToVar (fmap elHash asWitness)
--          isDone' <- gets (M.member cId . acSystem)
--          unless isDone' do
--            constraint (\x -> runPolynomial c (x . pure . elHash))
--            for_ (children asWitness) work
--        for_ cbLookups \(LEntry l t) -> do
--          lt <- lookupType t
--          isDone' <-
--            gets
--              ( any (S.member $ toList $ fmap elHash l)
--                  . (MM.!? lt)
--                  . acLookup
--              )
--          unless isDone' do
--            lookupConstraint (fmap (pure . elHash) l) t
--            for_ l work
--        for_ (children w) work
--      pure $ pure (elHash el)

compileOutput
  :: Functor o => Compiler a -> o (Witness a ZZp) -> CircuitContext a o
compileOutput Compiler {..} = crown circuitContext . fmap toVar

compile
  :: forall a c f
   . (Arithmetic a, Binary a, c ~ Node (Order a) ZZp, SymbolicFunction c f)
  => f -> ArithmeticCircuit a (Layout (Input f) c) (Layout (Output f) c)
compile =
  optimize . solder . \(f :: f) (l :: Layout (Input f) c NewVar) ->
    let (output, compiler) =
          flip runState makeCompiler
            . traverse compileNode
            . toLayout
            . symApply f
            $ fromLayout (fmap fromNewVar l)
     in compileOutput compiler output
 where
  fromNewVar :: NewVar -> Node (Order a) ZZp
  fromNewVar (EqVar v) = NodeInput v
  fromNewVar _ = error "folding not supported"
