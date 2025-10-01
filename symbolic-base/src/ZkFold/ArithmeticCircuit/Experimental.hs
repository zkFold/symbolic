{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{- HLINT ignore "Use record patterns" -}

module ZkFold.ArithmeticCircuit.Experimental where

import Control.DeepSeq (NFData (..), rwhnf)
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Function (const, ($), (.), flip)
import Data.Kind (Type)
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Type.Equality (type (~))
import GHC.Err (error)
import GHC.Generics (U1, (:*:) (..), Generic)
import GHC.Integer (Integer)
import GHC.TypeNats (KnownNat)
import Numeric.Natural (Natural)

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (Prime)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit, optimize, solder)
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Bool (BoolType (..))
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.Ord (IsOrdering (..), Ord (..))
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Compiler ()
import ZkFold.Symbolic.Data.V2 (Layout, SymbolicData (toLayout, fromLayout), HasRep)
import ZkFold.Symbolic.V2 (Constraint (..), Symbolic (..))
import ZkFold.ArithmeticCircuit.Var (NewVar (..), Var)
import ZkFold.ArithmeticCircuit.Context (emptyContext, crown, CircuitContext)
import Control.Monad.State (runState, State, gets)
import Data.Traversable (traverse, Traversable)
import Data.Functor (fmap)
import ZkFold.ArithmeticCircuit.Witness (EuclideanF, BooleanF, OrderingF)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Applicative (pure)
import Data.Bool (Bool(..))
import Control.Monad ((>>=))
import ZkFold.Symbolic.MonadCircuit (MonadCircuit(lookupConstraint, constraint))
import Optics (zoom)
import ZkFold.Algebra.Polynomial.Multivariate.Maps (traversePoly, evalPoly)
import qualified Data.Eq as Prelude
import qualified Data.Ord as Prelude

------------------- Experimental single-output circuit type --------------------

type Hash = ByteString

type Size = Maybe Natural

data Op (size :: Size) where
  OpConst :: Integer -> Op size
  OpScale :: Integer -> Node size -> Op size
  OpAdd, OpMul :: Node size -> Node size -> Op size
  OpNeg :: Node size -> Op size
  OpExp :: Node size -> Natural -> Op size
  OpFrom :: Node Nothing -> Op (Just n)
  OpTo :: Node (Just n) -> Op Nothing
  OpCompare :: Node Nothing -> Node Nothing -> Op (Just 3)
  OpDiv
    , OpMod
    , OpGcd
    , OpBezoutL
    , OpBezoutR
    :: Node Nothing -> Node Nothing -> Op Nothing
  OpInv :: Node (Just n) -> Op (Just n)
  OpEq, OpNEq :: Node size -> Node size -> Op (Just 2)
  OpOr :: Node (Just 2) -> Node (Just 2) -> Op (Just 2)
  OpBool :: Node size -> Node size -> Node (Just 2) -> Op size
  OpOrder :: Node size -> Node size -> Node size -> Node (Just 3) -> Op size

data Node (size :: Size) where
  NodeInput :: Hash -> Node (Just s)
  NodeApply :: Op size -> Hash -> Node size
  NodeConstrain
    :: s ~ Just size => Constraint (Node s) -> Node s -> Hash -> Node s

instance Prelude.Eq (Node s) where
  NodeInput h == NodeInput h' = h Prelude.== h'
  NodeApply _ h == NodeApply _ h' = h Prelude.== h'
  NodeConstrain _ _ h == NodeConstrain _ _ h' = h Prelude.== h'
  _ == _ = False

instance Prelude.Ord (Node s) where
  NodeInput h `compare` NodeInput h' = h `Prelude.compare` h'
  NodeInput _ `compare` _ = Prelude.LT
  NodeApply _ _ `compare` NodeInput _ = Prelude.GT
  NodeApply _ h `compare` NodeApply _ h' = h `Prelude.compare` h'
  NodeApply _ _ `compare` NodeConstrain _ _ _ = Prelude.LT
  NodeConstrain _ _ h `compare` NodeConstrain _ _ h' = h `Prelude.compare` h'
  NodeConstrain _ _ _ `compare` _ = Prelude.GT

instance NFData (Node s) where
  rnf = rwhnf -- GADTs are strict, so no need to eval

apply :: Op size -> Node size
apply op = NodeApply op (error "TODO")

instance Conditional (Node (Just 2)) (Node size) where
  bool onFalse onTrue condition = apply (OpBool onFalse onTrue condition)

instance BoolType (Node (Just 2)) where
  true = one
  false = zero
  not = negate
  (&&) = (*)
  xor = (+)
  x || y = apply (OpOr x y)

instance Semigroup (Node (Just 3)) where
  x <> y = apply (OpOrder x y x x)

instance Monoid (Node (Just 3)) where
  mempty = zero

instance IsOrdering (Node (Just 3)) where
  lt = fromConstant ((-1) :: Integer)
  eq = zero
  gt = one

instance Eq (Node s) where
  type BooleanOf (Node s) = Node (Just 2)
  x == y = apply (OpEq x y)
  x /= y = apply (OpNEq x y)

instance Ord (Node Nothing) where
  type OrderingOf (Node Nothing) = Node (Just 3)
  compare x y = apply (OpCompare x y)
  ordering x y z o = apply (OpOrder x y z o)
  x < y = compare x y == lt
  x <= y = compare x y /= gt
  x >= y = compare x y /= lt
  x > y = compare x y == gt

instance
  (KnownNat p, KnownNat (NumberOfBits (Node (Just p))))
  => Finite (Node (Just p))
  where
  type Order (Node (Just p)) = p

instance FromConstant Natural (Node s) where
  fromConstant x = apply $ OpConst (fromConstant x)

instance FromConstant Integer (Node s) where
  fromConstant x = apply (OpConst x)

instance FromConstant (Node Nothing) (Node (Just n)) where
  fromConstant x = apply (OpFrom x)

instance Scale Natural (Node s) where
  scale k x = apply (OpScale (fromConstant k) x)

instance Scale Integer (Node s) where
  scale k x = apply (OpScale k x)

instance Exponent (Node s) Natural where
  x ^ p = apply (OpExp x p)

instance Prime p => Exponent (Node (Just p)) Integer where
  (^) = intPowF

instance Zero (Node s) where
  zero = apply (OpConst 0)

instance AdditiveSemigroup (Node s) where
  x + y = apply (OpAdd x y)

instance AdditiveMonoid (Node s)

instance AdditiveGroup (Node s) where
  negate x = apply (OpNeg x)

instance MultiplicativeSemigroup (Node s) where
  x * y = apply (OpMul x y)

instance MultiplicativeMonoid (Node s) where
  one = apply (OpConst 1)

instance Semiring (Node s)

instance Ring (Node s)

instance SemiEuclidean (Node Nothing) where
  div x y = apply (OpDiv x y)
  mod x y = apply (OpMod x y)

instance Euclidean (Node Nothing) where
  gcd x y = apply (OpGcd x y)
  bezoutL x y = apply (OpBezoutL x y)
  bezoutR x y = apply (OpBezoutR x y)

instance Prime p => Field (Node (Just p)) where
  finv x = apply (OpInv x)

instance
  (Prime p, KnownNat (NumberOfBits (Node (Just p))))
  => PrimeField (Node (Just p))
  where
  type IntegralOf (Node (Just p)) = Node Nothing
  toIntegral x = apply (OpTo x)

instance
  (Prime p, KnownNat (NumberOfBits (Node (Just p))))
  => Symbolic (Node (Just p))
  where
  constrain c x = NodeConstrain c x (error "TODO")

------------------------- Optimized compilation function -----------------------

type family InputF (f :: Type) where
  InputF (i a -> f) = i :*: Input f
  InputF (o a) = U1

type family OutputF (f :: Type) where
  OutputF (i a -> f) = Output f
  OutputF (o a) = o

class
  ( SymbolicData (Input f), HasRep (Input f) a
  , SymbolicData (Output f), Traversable (Layout (Output f) a)
  ) => SymbolicFunction (a :: Type) (f :: Type) | f -> a where
  type Input f :: Type -> Type
  type Input f = InputF f
  type Output f :: Type -> Type
  type Output f = OutputF f
  symApply :: f -> Input f a -> Output f a

instance
  (SymbolicData o, Traversable (Layout o a), Input (o a) ~ U1, Output (o a) ~ o)
  => SymbolicFunction a (o a) where
  symApply = const

instance
  (SymbolicData i, HasRep i a, SymbolicFunction a f)
  => SymbolicFunction a (i a -> f) where
  symApply f (x :*: y) = symApply (f x) y

data Compiler a = Compiler
  { circuitContext :: CircuitContext a U1
  , seenConstraint :: Set Hash
  , integers :: Map Hash (EuclideanF a Hash)
  , booleans :: Map Hash (BooleanF a Hash)
  , orders :: Map Hash (OrderingF a Hash)
  }
  deriving (Generic)

makeCompiler :: Compiler a
makeCompiler = Compiler emptyContext S.empty M.empty M.empty M.empty

compileNode ::
  forall a. (Arithmetic a, Binary a) =>
  Node (Just (Order a)) -> State (Compiler a) (Var a)
compileNode (NodeInput v) = pure $ pure (EqVar v)
compileNode (NodeConstrain c n h) = do
  gets (S.member h . seenConstraint) >>= \case
    True -> pure ()
    False -> case c of
      Lookup lkp ns -> do
        vs <- traverse compileNode ns
        zoom #circuitContext (lookupConstraint vs lkp)
      Polynomial p -> do
        poly <- traversePoly @_ @a compileNode p
        zoom #circuitContext $ constraint (evalPoly poly)
  compileNode n
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

compileOutput :: Compiler a -> o (Var a) -> CircuitContext a o
compileOutput Compiler {..} = crown circuitContext

compile
  :: forall a c f
   . (Arithmetic a, Binary a, c ~ Node (Just (Order a)), SymbolicFunction c f)
  => f -> ArithmeticCircuit a (Layout (Input f) c) (Layout (Output f) c)
compile = optimize . solder . \(f :: f) (l :: Layout (Input f) c NewVar) ->
  let (output, compiler) = flip runState makeCompiler . traverse compileNode
                           . toLayout . symApply f
                           $ fromLayout (fmap fromNewVar l)
   in compileOutput compiler output
  where
    fromNewVar :: NewVar -> Node (Just size)
    fromNewVar (EqVar v) = NodeInput v
    fromNewVar _ = error "folding not supported"
