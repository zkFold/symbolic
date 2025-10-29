{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.ArithmeticCircuit.Elem where

import Control.Applicative (pure)
import Control.DeepSeq (NFData (..), rwhnf)
import Control.Monad (unless)
import Control.Monad.State (State, gets, modify', runState)
import Data.Binary (Binary)
import Data.Eq (Eq (..))
import Data.Foldable (Foldable (..), any, for_)
import Data.Function (const, on, ($), (.))
import Data.Functor (fmap)
import qualified Data.Map as M
import qualified Data.Map.Monoidal as MM
import Data.Maybe (Maybe (..), maybe)
import Data.Monoid (Monoid, mempty)
import Data.Ord (Ord (..))
import Data.Semigroup (Semigroup, (<>))
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import qualified Data.Set as S
import Data.Traversable (Traversable, traverse)
import Data.Type.Equality (type (~))
import GHC.Generics (Generic, U1)
import Numeric.Natural (Natural)
import Optics (over, zoom)
import Text.Show (Show (..))
import Prelude (error)

import ZkFold.Algebra.Class
import ZkFold.ArithmeticCircuit (ArithmeticCircuit, optimize, solder)
import qualified ZkFold.ArithmeticCircuit as AC
import ZkFold.ArithmeticCircuit.Children (children)
import ZkFold.ArithmeticCircuit.Context (
  CircuitContext,
  acLookup,
  acSystem,
  acWitness,
  crown,
  emptyContext,
  lookupType,
  witToVar,
 )
import ZkFold.ArithmeticCircuit.Node (Input, Output, SymbolicFunction, symApply)
import ZkFold.ArithmeticCircuit.Var (NewVar (..), Var)
import ZkFold.ArithmeticCircuit.Witness (BooleanF, EuclideanF, WitnessF (..))
import ZkFold.Control.Conditional (Conditional (..))
import qualified ZkFold.Data.Eq as ZkFold
import ZkFold.Data.HFunctor (HFunctor (hmap))
import ZkFold.Data.Product (fstP)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Compat (CompatContext, CompatData (CompatData))
import ZkFold.Symbolic.Data.Class (LayoutFunctor)
import ZkFold.Symbolic.Data.V2 (Layout, SymbolicData (fromLayout), toLayout)
import ZkFold.Symbolic.Data.Vec (Vec (Vec))
import ZkFold.Symbolic.MonadCircuit (MonadCircuit (..))
import ZkFold.Symbolic.V2 (Constraint (..), LookupTable, Symbolic (..))

---------------------- Efficient "list" concatenation --------------------------

newtype AppList a = AList {aList :: forall b. (a -> b -> b) -> b -> b}

app :: a -> AppList a -> AppList a
app x (AList a) = AList \f s -> f x (a f s)

instance NFData (AppList a) where
  rnf = rwhnf

instance Show a => Show (AppList a) where
  show = show . toList

instance Semigroup (AppList a) where
  AList a <> AList b = AList \f s -> a f (b f s)

instance Monoid (AppList a) where
  mempty = AList \_ s -> s

instance Foldable AppList where
  foldr f s (AList a) = a f s

----------------- Compact polynomial constraint representation -----------------

newtype Polynomial a v = MkPolynomial
  {runPolynomial :: forall b. Algebra a b => (v -> b) -> b}

instance (PrimeField a, Ord v, Show v) => Show (Polynomial a v) where
  show (MkPolynomial f) = "p(" <> show (children @a $ WitnessF f) <> ")"

instance {-# OVERLAPPING #-} FromConstant v (Polynomial a v) where
  fromConstant v = MkPolynomial ($ v)

instance {-# OVERLAPPING #-} Scale v (Polynomial a v) where
  scale v (MkPolynomial f) = MkPolynomial \x -> x v * f x

instance FromConstant c a => FromConstant c (Polynomial a v) where
  fromConstant c = MkPolynomial \_ -> fromConstant (fromConstant c :: a)

instance (Scale c a, MultiplicativeMonoid a) => Scale c (Polynomial a v) where
  scale k (MkPolynomial f) = MkPolynomial (scale (scale k one :: a) . f)

instance Exponent (Polynomial a v) Natural where
  MkPolynomial x ^ p = MkPolynomial (x ^ p)

instance {-# OVERLAPPING #-} FromConstant (Polynomial a v) (Polynomial a v)

instance {-# OVERLAPPING #-} Scale (Polynomial a v) (Polynomial a v)

instance MultiplicativeSemigroup (Polynomial a v) where
  MkPolynomial f * MkPolynomial g = MkPolynomial (f * g)

instance MultiplicativeMonoid (Polynomial a v) where
  one = MkPolynomial (const one)

instance Zero (Polynomial a v) where
  zero = MkPolynomial (const zero)

instance AdditiveSemigroup (Polynomial a v) where
  MkPolynomial f + MkPolynomial g = MkPolynomial (f + g)

instance Semiring a => AdditiveMonoid (Polynomial a v)

instance Ring a => AdditiveGroup (Polynomial a v) where
  negate (MkPolynomial f) = MkPolynomial (negate f)

instance Semiring a => Semiring (Polynomial a v)

instance Ring a => Ring (Polynomial a v)

--------------- Type-preserving lookup constraint representation ---------------

data LookupEntry v = forall f. Traversable f => LEntry (f v) (LookupTable f)

instance Show v => Show (LookupEntry v) where
  show (LEntry x _) = "l(" <> show (toList x) <> ")"

------------- Box of constraints supporting efficient concatenation ------------

data ConstraintBox a v = MkCBox
  { cbPolyCon :: AppList (Polynomial a v)
  , cbLookups :: AppList (LookupEntry v)
  }
  deriving (Generic, NFData, Show)
  deriving (Monoid, Semigroup) via (GenericSemigroupMonoid (ConstraintBox a v))

------------------- Experimental single-output circuit type --------------------

-- | If this approach works out, we can simplify Symbolic interface down to:
-- * a mysterious "field" type (here: Elem a);
-- * a way to constrain elements of this field via generic 'constrain' function.
data Elem a = MkElem
  { elHash :: NewVar
  , elWitness :: Maybe (WitnessF a (Elem a))
  , elConstraints :: ConstraintBox a (Elem a)
  }
  deriving (Generic, NFData)

fromVar :: NewVar -> Elem a
fromVar v = MkElem v Nothing mempty

instance Eq (Elem a) where
  (==) = (==) `on` elHash

instance Ord (Elem a) where
  compare = compare `on` elHash

instance (PrimeField a, Show a) => Show (Elem a) where
  show MkElem {..} =
    "{ elHash = "
      <> show elHash
      <> ", elWitness = "
      <> maybe
        "<input>"
        (\w -> "f(" <> show (children w) <> ")")
        elWitness
      <> ", elConstraints = "
      <> show elConstraints
      <> "}"

instance
  (Arithmetic a, Binary a, FromConstant c (WitnessF a (Elem a)))
  => FromConstant c (Elem a)
  where
  fromConstant (fromConstant -> witness) =
    MkElem
      { elHash = EqVar $ witToVar @a (fmap elHash witness)
      , elConstraints = mempty
      , elWitness = Just witness
      }

instance ZkFold.Eq (Elem a) where
  type BooleanOf (Elem a) = BooleanF a (Elem a)
  x == y = (pure x :: WitnessF a (Elem a)) ZkFold.== pure y
  x /= y = (pure x :: WitnessF a (Elem a)) ZkFold./= pure y

instance (Arithmetic a, Binary a) => Conditional (BooleanF a (Elem a)) (Elem a) where
  bool x y b = fromConstant (bool (pure x :: WitnessF a (Elem a)) (pure y) b)

instance Finite a => Finite (Elem a) where
  type Order (Elem a) = Order a

instance
  (Arithmetic a, Binary a, Exponent (WitnessF a (Elem a)) e)
  => Exponent (Elem a) e
  where
  x ^ e = fromConstant (pure x ^ e :: WitnessF a (Elem a))

instance
  (Arithmetic a, Binary a, Scale k (WitnessF a (Elem a)))
  => Scale k (Elem a)
  where
  scale k = fromConstant . scale k . pure @(WitnessF a)

instance {-# OVERLAPPING #-} FromConstant (Elem a) (Elem a)

instance {-# OVERLAPPING #-} (Arithmetic a, Binary a) => Scale (Elem a) (Elem a)

instance (Arithmetic a, Binary a) => Zero (Elem a) where
  zero = fromConstant @(WitnessF a (Elem a)) zero

instance (Arithmetic a, Binary a) => AdditiveSemigroup (Elem a) where
  x + y = fromConstant (pure @(WitnessF a) x + pure y)

instance (Arithmetic a, Binary a) => AdditiveMonoid (Elem a)

instance (Arithmetic a, Binary a) => AdditiveGroup (Elem a) where
  negate = fromConstant . negate . pure @(WitnessF a)

instance (Arithmetic a, Binary a) => MultiplicativeSemigroup (Elem a) where
  x * y = fromConstant (pure @(WitnessF a) x * pure y)

instance (Arithmetic a, Binary a) => MultiplicativeMonoid (Elem a) where
  one = fromConstant @(WitnessF a (Elem a)) one

instance (Arithmetic a, Binary a) => Semiring (Elem a)

instance (Arithmetic a, Binary a) => Ring (Elem a)

instance (Arithmetic a, Binary a) => Field (Elem a) where
  finv = fromConstant . finv . pure @(WitnessF a)

instance (Arithmetic a, Binary a) => PrimeField (Elem a) where
  type IntegralOf (Elem a) = EuclideanF a (Elem a)
  toIntegral = toIntegral @(WitnessF a (Elem a)) . pure

instance (Arithmetic a, Binary a) => Symbolic (Elem a) where
  constrain =
    over #elConstraints . \case
      Polynomial p -> over #cbPolyCon $ app p
      Lookup l x -> over #cbLookups $ app $ LEntry x l

------------------------- Optimized compilation function -----------------------

exec
  :: (Arithmetic a, Binary a, LayoutFunctor f)
  => CompatContext (Elem a) f -> f a
exec = AC.exec . hmap fstP . compileV2 . CompatData . Vec

compileV2
  :: forall a c f
   . (Arithmetic a, Binary a, c ~ Elem a, SymbolicFunction c f)
  => f -> ArithmeticCircuit a (Layout (Input f) c) (Layout (Output f) c)
compileV2 =
  optimize . solder . \(f :: f) (l :: Layout (Input f) c NewVar) ->
    let output = toLayout $ symApply f $ fromLayout (fmap fromVar l)
        (vars, circuit) = runState (traverse work output) emptyContext
     in crown circuit vars
 where
  work :: Elem a -> State (CircuitContext a U1) (Var a)
  work el = case (elWitness el, elHash el) of
    (Nothing, v) -> pure (pure v) -- input variable
    (_, FoldPVar _ _) -> error "TODO: fold constraints"
    (_, FoldLVar _ _) -> error "TODO: fold constraints"
    (Just w, EqVar bs) -> do
      isDone <- gets (M.member bs . acWitness)
      unless isDone do
        zoom #acWitness $ modify' $ M.insert bs (fmap elHash w)
        let MkCBox {..} = elConstraints el
        for_ cbPolyCon \c -> do
          let asWitness = WitnessF @a (runPolynomial c)
              cId = witToVar (fmap elHash asWitness)
          isDone' <- gets (M.member cId . acSystem)
          unless isDone' do
            constraint (\x -> runPolynomial c (x . pure . elHash))
            for_ (children asWitness) work
        for_ cbLookups \(LEntry l t) -> do
          lt <- lookupType t
          isDone' <-
            gets
              ( any (S.member $ toList $ fmap elHash l)
                  . (MM.!? lt)
                  . acLookup
              )
          unless isDone' do
            lookupConstraint (fmap (pure . elHash) l) t
            for_ l work
        for_ (children w) work
      pure $ pure (elHash el)
