{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- The purpose of this module is to provide compatibility
-- between Old Symbolic and Symbolic V2. In particular:
--
-- \* If `V2.Symbolic c`, then `Old.Symbolic (CompatContext c)`.
-- \* If `Old.SymbolicData f`, then `V2.SymbolicData (CompatData f)`.

module ZkFold.Symbolic.Compat where

import Control.Applicative (pure)
import Control.DeepSeq (NFData (..), NFData1, liftRnf)
import Control.Monad.State (State, modify', runState)
import Data.Bifunctor (bimap)
import qualified Data.Eq as Prelude
import Data.Foldable (foldr)
import Data.Function (flip, on, ($), (.))
import Data.Functor (Functor, fmap, (<$>))
import Data.Functor.Identity (Identity (Identity, runIdentity), runIdentity)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Type.Equality (type (~))
import GHC.Generics (Par1 (..), (:*:) (..))
import GHC.TypeLits (KnownNat)
import Numeric.Natural (Natural)
import Text.Show (Show (showList, showsPrec))

import ZkFold.Algebra.Class
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number (Prime)
import ZkFold.Control.HApplicative (HApplicative (..))
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.HFunctor (HFunctor (..))
import ZkFold.Data.HFunctor.Classes (HEq (..), HNFData (..), HShow (..))
import ZkFold.Data.Package (Package (..))
import qualified ZkFold.Symbolic.Class as Old
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (..), Conditional (..))
import qualified ZkFold.Symbolic.Data.Class as Old
import ZkFold.Symbolic.Data.V2 (SymbolicData (..))
import ZkFold.Symbolic.Interpreter (Interpreter (..))
import ZkFold.Symbolic.MonadCircuit (MonadCircuit (..))
import ZkFold.Symbolic.V2 (Constraint (..), Symbolic, constrain)
import ZkFold.Symbolic.Data.Ord (Ordering, Ord (..), IsOrdering)
import Data.Monoid (Monoid)
import Data.Semigroup (Semigroup)

newtype CompatData f c = CompatData {compatData :: f (CompatContext c)}

deriving instance Prelude.Eq (f (CompatContext c)) => Prelude.Eq (CompatData f c)

deriving instance NFData (f (CompatContext c)) => NFData (CompatData f c)

deriving instance Show (f (CompatContext c)) => Show (CompatData f c)

deriving instance Semigroup (f (CompatContext c)) => Semigroup (CompatData f c)

deriving instance Monoid (f (CompatContext c)) => Monoid (CompatData f c)

deriving instance
  ( BoolType (f (CompatContext c))
  , Conditional (CompatData f c) (CompatData f c)
  )
  => BoolType (CompatData f c)

deriving instance IsOrdering (f (CompatContext c)) => IsOrdering (CompatData f c)

deriving instance Scale k (f (CompatContext c)) => Scale k (CompatData f c)

deriving instance Zero (f (CompatContext c)) => Zero (CompatData f c)

instance {-# OVERLAPPING #-} FromConstant (CompatData f c) (CompatData f c)

deriving instance
  AdditiveSemigroup (f (CompatContext c)) => AdditiveSemigroup (CompatData f c)

deriving instance
  AdditiveMonoid (f (CompatContext c)) => AdditiveMonoid (CompatData f c)

deriving instance
  AdditiveGroup (f (CompatContext c)) => AdditiveGroup (CompatData f c)

instance
  {-# OVERLAPPING #-}
  MultiplicativeSemigroup (f (CompatContext c))
  => Scale (CompatData f c) (CompatData f c)

instance Exponent (f (CompatContext c)) e => Exponent (CompatData f c) e where
  CompatData x ^ e = CompatData (x ^ e)

deriving instance
  MultiplicativeSemigroup (f (CompatContext c))
  => MultiplicativeSemigroup (CompatData f c)

deriving instance
  MultiplicativeMonoid (f (CompatContext c))
  => MultiplicativeMonoid (CompatData f c)

deriving instance
  MultiplicativeGroup (f (CompatContext c))
  => MultiplicativeGroup (CompatData f c)

deriving instance
  FromConstant a (f (CompatContext c)) => FromConstant a (CompatData f c)

deriving instance Semiring (f (CompatContext c)) => Semiring (CompatData f c)

deriving instance Ring (f (CompatContext c)) => Ring (CompatData f c)

deriving instance
  ( Field (f (CompatContext c))
  , BooleanOf (f (CompatContext c)) ~ Bool (CompatContext c)
  )
  => Field (CompatData f c)

instance
  ( KnownNat (Order (f (CompatContext c)))
  , KnownNat (NumberOfBits (f (CompatContext c)))
  )
  => Finite (CompatData f c)
  where
  type Order (CompatData f c) = Order (f (CompatContext c))

instance (Old.Arithmetic a, Old.SymbolicData f, ToConstant (f (Interpreter a))) => ToConstant (CompatData f a) where
  type Const (CompatData f a) = Const (f (Interpreter a))
  toConstant = toConstant @(f (Interpreter a)) . mapContext . compatData

mapContext
  :: (Old.Arithmetic a, Old.SymbolicData f)
  => f (CompatContext a) -> f (Interpreter a)
mapContext x =
  Old.restore (Interpreter $ compatContext (Old.arithmetize x), Old.payload x)

instance
  ( Old.SymbolicData f
  , forall c. Old.HasRep f (CompatContext c) => Old.RepData f (CompatContext c)
  )
  => SymbolicData (CompatData f)
  where
  type Layout (CompatData f) c = Old.Layout f (Order c) :*: Old.Payload f (Order c)
  type HasRep (CompatData f) c = Old.HasRep f (CompatContext c)
  toLayout (CompatData f) = compatContext (Old.arithmetize f) :*: Old.payload f
  interpolate c bs =
    CompatData $
      Old.interpolate (bimap fromConstant compatData <$> bs) $
        CompatContext (Par1 c)
  fromLayout (layout :*: payload) =
    CompatData $ Old.restore (CompatContext layout, payload)

instance (SymbolicData f, Symbolic c) => Conditional (CompatData Bool c) (f c) where
  bool x y (CompatData (Bool (CompatContext (Par1 b)))) =
    interpolate b ((0, x) :| [(1, y)])

instance
  {-# OVERLAPPING #-}
  Conditional (Bool (CompatContext c)) (f (CompatContext c))
  => Conditional (CompatData Bool c) (CompatData f c)
  where
  bool (CompatData x) (CompatData y) (CompatData b) = CompatData (bool x y b)

instance
  ( Eq (f (CompatContext c))
  , BooleanOf (f (CompatContext c)) ~ Bool (CompatContext c)
  )
  => Eq (CompatData f c)
  where
  type BooleanOf (CompatData f c) = CompatData Bool c
  CompatData x == CompatData y = CompatData (x == y)
  CompatData x /= CompatData y = CompatData (x /= y)

instance
  ( Ord (f (CompatContext c))
  , BooleanOf (f (CompatContext c)) ~ Bool (CompatContext c)
  , OrderingOf (f (CompatContext c)) ~ Ordering (CompatContext c)
  ) => Ord (CompatData f c)
  where
  type OrderingOf (CompatData f c) = CompatData Ordering c
  CompatData x < CompatData y = CompatData (x < y)
  CompatData x <= CompatData y = CompatData (x <= y)
  CompatData x >= CompatData y = CompatData (x >= y)
  CompatData x > CompatData y = CompatData (x > y)
  compare (CompatData x) (CompatData y) = CompatData (compare x y)
  ordering (CompatData x) (CompatData y) (CompatData z) (CompatData w) =
    CompatData (ordering x y z w)

newtype CompatContext c f = CompatContext {compatContext :: f c}
  deriving (Prelude.Eq, Show)

instance Prelude.Eq c => HEq (CompatContext c) where
  hliftEq liftEq = liftEq (Prelude.==) `on` compatContext

instance Show c => HShow (CompatContext c) where
  hliftShowsPrec liftShowsPrec _ prec =
    liftShowsPrec showsPrec showList prec . compatContext

instance (NFData c, NFData1 f) => NFData (CompatContext c f) where
  rnf = hliftRnf liftRnf

instance NFData c => HNFData (CompatContext c) where
  hliftRnf lift (CompatContext f) = lift rnf f

instance HFunctor (CompatContext c) where
  hmap f = CompatContext . f . compatContext

instance HApplicative (CompatContext c) where
  hpure = CompatContext
  hliftA2 f (CompatContext x) (CompatContext y) = CompatContext (f x y)

instance Package (CompatContext c) where
  packWith f = CompatContext . f . fmap compatContext
  unpackWith f = fmap CompatContext . f . compatContext

instance (Prime p, KnownNat (NumberOfBits (Zp p))) => Symbolic (Zp p) where
  constrain _ x = x

instance Symbolic c => Old.Symbolic (CompatContext c) where
  type BaseField (CompatContext c) = Zp (Order c)
  type WitnessField (CompatContext c) = c
  witnessF = compatContext
  fromCircuitF (CompatContext x) f =
    CompatContext $ collect $ fmap runIdentity <$> f (Identity <$> x)
   where
    collect :: Functor f => State [Constraint c] (f c) -> f c
    collect fs =
      let (xs, cs) = runState fs [] in flip (foldr constrain) cs <$> xs

instance
  (Symbolic c, Order c ~ n)
  => MonadCircuit (Identity c) (Zp n) c (State [Constraint c])
  where
  unconstrained = pure . Identity
  constraint f =
    modify' (Polynomial (compatAlgebra $ f (fromConstant . runIdentity)) :)
  lookupConstraint (fmap runIdentity -> xs) lt = modify' (Lookup lt xs :)

newtype CompatAlgebra a = CompatAlgebra {compatAlgebra :: a}
  deriving
    ( AdditiveGroup
    , AdditiveSemigroup
    , Functor
    , MultiplicativeMonoid
    , MultiplicativeSemigroup
    , Zero
    )

deriving instance
  {-# OVERLAPPABLE #-}
  FromConstant b a => FromConstant b (CompatAlgebra a)

deriving instance {-# OVERLAPPABLE #-} Scale b a => Scale b (CompatAlgebra a)

deriving instance AdditiveMonoid a => AdditiveMonoid (CompatAlgebra a)

deriving instance Semiring a => Semiring (CompatAlgebra a)

deriving instance Ring a => Ring (CompatAlgebra a)

deriving instance Finite a => Finite (CompatAlgebra a)

instance {-# OVERLAPPING #-} FromConstant (CompatAlgebra a) (CompatAlgebra a)

instance {-# OVERLAPPING #-} MultiplicativeSemigroup a => Scale (CompatAlgebra a) (CompatAlgebra a)

instance Exponent a b => Exponent (CompatAlgebra a) b where
  CompatAlgebra x ^ p = CompatAlgebra (x ^ p)

instance {-# OVERLAPPING #-} Scale Natural a => Scale (Zp n) (CompatAlgebra a) where
  scale = scale . toConstant

instance {-# OVERLAPPING #-} FromConstant Natural a => FromConstant (Zp n) (CompatAlgebra a) where
  fromConstant = fromConstant . toConstant

----------------------------------- ORPHANS ------------------------------------

instance {-# INCOHERENT #-} (FromConstant Natural a, Order a ~ n) => FromConstant (Zp n) a where
  fromConstant = fromConstant . toConstant

instance {-# INCOHERENT #-} (Scale Natural a, Order a ~ n) => Scale (Zp n) a where
  scale = scale . toConstant
