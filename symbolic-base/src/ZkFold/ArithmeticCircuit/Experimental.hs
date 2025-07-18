{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.ArithmeticCircuit.Experimental where

import Control.Applicative (pure)
import Control.DeepSeq (NFData (..), NFData1, liftRnf, rwhnf)
import Control.Monad.State (State, modify', runState, state)
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Function (flip, ($), (.))
import Data.Functor (Functor, fmap)
import Data.Map (Map)
import Data.Monoid (Monoid, mempty)
import Data.Semigroup (Semigroup, (<>))
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.Tuple (swap, uncurry)
import GHC.Generics (Generic)
import Prelude (seq)

import ZkFold.Algebra.Class
import ZkFold.ArithmeticCircuit.Context (LookupFunction, appendFunction, witToVar)
import ZkFold.ArithmeticCircuit.Lookup (LookupTable)
import ZkFold.ArithmeticCircuit.Var (NewVar (..))
import ZkFold.ArithmeticCircuit.Witness (WitnessF)
import ZkFold.Control.HApplicative (HApplicative (..))
import ZkFold.Data.HFunctor (HFunctor (..))
import ZkFold.Data.HFunctor.Classes (HNFData (..))
import ZkFold.Data.Package (Package (..))
import ZkFold.Symbolic.Class (Arithmetic, Symbolic (..))
import ZkFold.Symbolic.MonadCircuit (MonadCircuit (..), Witness (..))

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

----------------- Compact polynomial constraint representation -----------------

newtype Polynomial a v = MkPolynomial
  {runPolynomial :: forall b. Algebra a b => (v -> b) -> b}

instance NFData (Polynomial a v) where
  rnf = rwhnf

--------------- Type-preserving lookup constraint representation ---------------

data LookupEntry a v
  = forall f. NFData1 f => LEntry (f v) (LookupTable a f)

instance (NFData a, NFData v) => NFData (LookupEntry a v) where
  rnf (LEntry v t) = liftRnf rnf v `seq` rnf t

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

-- If this approach works out, we can simplify Symbolic interface down to:

-- * a mysterious "field" type (here: Elem a);

-- * a way to constrain elements of this field via generic 'constrain' function.

data Elem a = MkElem
  { elHash :: ByteString
  , elWitness :: WitnessF a (Elem a)
  , elConstraints :: ConstraintBox a (Elem a)
  }
  deriving (Generic, NFData)

-- A 'constrain' function applies constraints to the "field element".
constrain :: ConstraintBox a (Elem a) -> Elem a -> Elem a
constrain cb el = el {elConstraints = cb <> elConstraints el}

instance
  (Arithmetic a, Binary a, FromConstant c (WitnessF a (Elem a)))
  => FromConstant c (Elem a)
  where
  fromConstant (fromConstant -> (elWitness :: WitnessF a (Elem a))) =
    MkElem
      { elHash = witToVar $ fmap (EqVar . elHash) elWitness
      , elConstraints = mempty
      , ..
      }

--------------------- Adapters into current Symbolic API -----------------------

newtype AC a f = MkAC {runAC :: f (Elem a)}

deriving newtype instance NFData1 f => NFData (AC a f)

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
