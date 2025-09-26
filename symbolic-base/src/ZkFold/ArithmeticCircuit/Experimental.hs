{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.ArithmeticCircuit.Experimental where

import Control.DeepSeq (NFData (..), rwhnf)
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Function (const, ($), (.))
import Data.Functor.Rep (Rep)
import Data.Kind (Type)
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Type.Equality (type (~))
import GHC.Err (error)
import GHC.Generics (U1, (:*:) (..))
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
import ZkFold.Symbolic.Data.V2 (Layout, SymbolicData (HasRep))
import ZkFold.Symbolic.V2 (Constraint, Symbolic (..))

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
  NodeApply :: Op size -> Hash -> Node size
  NodeConstrain
    :: s ~ Just size => Constraint (Node s) -> Node s -> Hash -> Node s

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

type family Input (f :: Type) where
  Input (i a -> f) = i :*: Input f
  Input (o a) = U1

type family Output (f :: Type) where
  Output (i a -> f) = Output f
  Output (o a) = o

class
  (SymbolicData (Input f), SymbolicData (Output f)) =>
  SymbolicFunction (a :: Type) (f :: Type)
    | f -> a
  where
  symApply :: f -> Input f a -> Output f a

instance
  (SymbolicData (Input (o a)), SymbolicData o, Output (o a) ~ o)
  => SymbolicFunction a (o a)
  where
  symApply = const

instance (SymbolicData i, SymbolicFunction a f) => SymbolicFunction a (i a -> f) where
  symApply f (x :*: y) = symApply (f x) y

compile
  :: (Arithmetic a, Binary a, Binary (Rep (Layout (Input f) c)), c ~ Node (Just (Order a)), SymbolicFunction c f)
  => f -> ArithmeticCircuit a (Layout (Input f) c) (Layout (Output f) c)
compile =
  optimize
    . solder
    . \f l -> _

-- compile
--  :: forall a s f n
--   . ( SymbolicFunction f
--     , Context f ~ AC a
--     , Domain f ~ s
--     , Representable (Layout s n)
--     , Representable (Payload s n)
--     , Binary (Rep (Layout s n))
--     , Binary (Rep (Payload s n))
--     , Binary a
--     , n ~ Order a
--     )
--  => f -> ArithmeticCircuit a (Payload s n :*: Layout s n) (Layout (Range f) n)
-- compile =
--  optimize . solder . \f (p :*: l) ->
--    let input = restore (MkAC (fmap fromVar l), fmap (pure . fromVar) p)
--        Bool b = isValid input
--        output = apply f input
--        MkAC constrained = fromCircuit2F (arithmetize output) b \r (Par1 i) -> do
--          constraint (one - ($ i))
--          pure r
--        (vars, circuit) = runState (traverse work constrained) emptyContext
--     in crown circuit vars
-- where
--  work :: Elem a -> State (CircuitContext a U1) (Var a)
--  work el = case (elWitness el, elHash el) of
--    (Nothing, v) -> pure (pure v) -- input variable
--    (_, FoldPVar _ _) -> error "TODO: fold constraints"
--    (_, FoldLVar _ _) -> error "TODO: fold constraints"
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
