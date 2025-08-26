{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.ArithmeticCircuit.Experimental where

import Control.Applicative (pure)
import Control.DeepSeq (NFData (..), NFData1, liftRnf, rwhnf)
import Control.Monad (unless)
import Control.Monad.State (State, gets, modify', runState, state)
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Eq (Eq (..))
import Data.Foldable (Foldable (..), any, for_)
import Data.Function (flip, on, ($), (.))
import Data.Functor (Functor, fmap)
import Data.Functor.Rep (Rep, Representable)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Monoidal as MM
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid, mempty)
import Data.Ord (Ord (..))
import Data.Semigroup (Semigroup, (<>))
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import qualified Data.Set as S
import Data.Traversable (traverse)
import Data.Tuple (swap, uncurry)
import Data.Type.Equality (type (~))
import Data.Typeable (Typeable, Proxy (Proxy))
import GHC.Generics (Generic, Par1 (..), U1, (:*:) (..))
import Optics (zoom)
import Prelude (error)

import ZkFold.Algebra.Class
import ZkFold.ArithmeticCircuit (ArithmeticCircuit, optimize, solder)
import ZkFold.ArithmeticCircuit.Children (children)
import ZkFold.ArithmeticCircuit.Context (
  CircuitContext,
  LookupFunction (LookupFunction),
  acLookup,
  acSystem,
  acWitness,
  appendFunction,
  crown,
  emptyContext,
  witToVar,
 )
import ZkFold.ArithmeticCircuit.Lookup (LookupTable, LookupType (..))
import ZkFold.ArithmeticCircuit.Var (NewVar (..), Var)
import ZkFold.ArithmeticCircuit.Witness (WitnessF (..))
import ZkFold.Control.HApplicative (HApplicative (..))
import ZkFold.Data.HFunctor (HFunctor (..))
import ZkFold.Data.HFunctor.Classes (HNFData (..))
import ZkFold.Data.Package (Package (..))
import ZkFold.Symbolic.Class (
  Arithmetic,
  Symbolic (..),
  fromCircuit2F,
 )
import ZkFold.Symbolic.Compiler (SymbolicFunction (..))
import ZkFold.Symbolic.Data.Bool (Bool (..))
import ZkFold.Symbolic.Data.Class (
  Layout,
  Payload,
  arithmetize,
  restore, dataFunctor,
 )
import ZkFold.Symbolic.Data.Input (isValid)
import ZkFold.Symbolic.MonadCircuit (MonadCircuit (..), Witness (..))
import Data.Constraint (withDict)

---------------------- Efficient "list" concatenation --------------------------

newtype AppList a = AList {aList :: forall b. (a -> b -> b) -> b -> b}

app :: a -> AppList a -> AppList a
app x (AList a) = AList \f s -> f x (a f s)

instance NFData (AppList a) where
  rnf = rwhnf

instance Semigroup (AppList a) where
  AList a <> AList b = AList \f s -> a f (b f s)

instance Monoid (AppList a) where
  mempty = AList \_ s -> s

instance Foldable AppList where
  foldr f s (AList a) = a f s

----------------- Compact polynomial constraint representation -----------------

newtype Polynomial a v = MkPolynomial
  {runPolynomial :: forall b. Algebra a b => (v -> b) -> b}

--------------- Type-preserving lookup constraint representation ---------------

data LookupEntry a v
  = forall f.
    (Functor f, Foldable f, NFData1 f, Typeable f) =>
    LEntry (f v) (LookupTable a f)

------------- Box of constraints supporting efficient concatenation ------------

-- After #573, can be made even more declarative
-- by getting rid of 'cbLkpFuns' field.
-- Can then be used for new public Symbolic API (see 'constrain' below)!
data ConstraintBox a v = MkCBox
  { cbPolyCon :: AppList (Polynomial a v)
  , cbLkpFuns :: Map ByteString (LookupFunction a)
  , cbLookups :: AppList (LookupEntry a v)
  }
  deriving (Generic, NFData)
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

-- A 'constrain' function applies constraints to the "field element".
constrain :: ConstraintBox a (Elem a) -> Elem a -> Elem a
constrain cb el = el {elConstraints = cb <> elConstraints el}

instance Eq (Elem a) where
  (==) = (==) `on` elHash

instance Ord (Elem a) where
  compare = compare `on` elHash

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

--------------------- Adapters into current Symbolic API -----------------------

newtype AC a f = MkAC {runAC :: f (Elem a)}

instance NFData1 f => NFData (AC a f) where
  rnf = hliftRnf liftRnf

instance HNFData (AC a) where
  hliftRnf lift = lift rnf . runAC

instance HFunctor (AC a) where
  hmap f = MkAC . f . runAC

instance HApplicative (AC a) where
  hpure = MkAC
  hliftA2 f (MkAC a) (MkAC b) = MkAC (f a b)

instance Package (AC a) where
  unpackWith f = fmap MkAC . f . runAC
  packWith f = MkAC . f . fmap runAC

instance (Arithmetic a, Binary a) => Symbolic (AC a) where
  type BaseField (AC a) = a
  type WitnessField (AC a) = WitnessF a (Elem a)
  witnessF = fmap at . runAC
  fromCircuitF (MkAC es) cf = MkAC $ commit (cf es)

commit
  :: Functor f => State (ConstraintBox a (Elem a)) (f (Elem a)) -> f (Elem a)
commit = uncurry (fmap . constrain) . swap . flip runState mempty

instance Arithmetic a => Witness (Elem a) (WitnessF a (Elem a)) where
  at = pure

instance
  (Arithmetic a, Binary a)
  => MonadCircuit (Elem a) a (WitnessF a (Elem a)) (State (ConstraintBox a (Elem a)))
  where
  unconstrained = pure . fromConstant
  constraint c = modify' \cb -> cb {cbPolyCon = MkPolynomial c `app` cbPolyCon cb}
  registerFunction f = state \(!cb) ->
    let (i, r') = appendFunction f (cbLkpFuns cb)
     in (i, cb {cbLkpFuns = r'})
  lookupConstraint c t = modify' \cb -> cb {cbLookups = LEntry c t `app` cbLookups cb}

------------------------- Optimized compilation function -----------------------

compile
  :: forall a s f n
   . ( SymbolicFunction f
     , Context f ~ AC a
     , Domain f ~ s
     , Representable (Layout s n)
     , Representable (Payload s n)
     , Binary (Rep (Layout s n))
     , Binary (Rep (Payload s n))
     , Binary a
     , n ~ Order a
     )
  => f -> ArithmeticCircuit a (Payload s n :*: Layout s n) (Layout (Range f) n)
compile = withDict (dataFunctor @(Range f) @n Proxy) $
  optimize . solder . \f (p :*: l) ->
    let input = restore (MkAC (fmap fromVar l), fmap (pure . fromVar) p)
        Bool b = isValid input
        output = apply f input
        MkAC constrained = fromCircuit2F (arithmetize output) b \r (Par1 i) -> do
          constraint (one - ($ i))
          pure r
        (vars, circuit) = runState (traverse work constrained) emptyContext
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
        for_ cbLkpFuns \(LookupFunction f) -> do
          _ <- registerFunction f
          pure ()
        for_ cbLookups \(LEntry l t) -> do
          isDone' <-
            gets
              ( any (S.member $ toList $ fmap elHash l)
                  . (MM.!? LookupType t)
                  . acLookup
              )
          unless isDone' do
            lookupConstraint (fmap (pure . elHash) l) t
            for_ l work
        for_ (children w) work
      pure $ pure (elHash el)
