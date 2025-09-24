{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- The purpose of this module is to provide compatibility
-- between Old Symbolic and Symbolic V2. In particular:
--
-- * If `V2.Symbolic c`, then `Old.Symbolic (CompatContext c)`.
-- * If `Old.SymbolicData f`, then `V2.SymbolicData (CompatData f)`.

module ZkFold.Symbolic.Compat where

import ZkFold.Symbolic.V2 (Symbolic, Constraint (..), constrain)
import qualified ZkFold.Symbolic.Class as Old
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Class
import Control.DeepSeq (NFData, rnf)
import Numeric.Natural (Natural)
import Data.Function ((.), ($), flip)
import Data.Type.Equality (type (~))
import ZkFold.Control.HApplicative (HApplicative (..))
import ZkFold.Data.HFunctor (HFunctor (..))
import ZkFold.Data.HFunctor.Classes (HNFData (..))
import ZkFold.Data.Package (Package (..))
import Data.Foldable (foldr)
import Data.Functor (fmap, Functor, (<$>))
import Control.Applicative (pure)
import ZkFold.Symbolic.MonadCircuit (MonadCircuit (..), Witness (..))
import Control.Monad.State (State, modify', runState)
import qualified ZkFold.Symbolic.Data.Class as Old
import ZkFold.Symbolic.Data.V2 (SymbolicData (..))
import GHC.Generics ((:*:) (..), Par1 (..))
import Data.Bifunctor (bimap)

newtype CompatData f c = CompatData { compatData :: f (CompatContext c) }

instance
  ( Old.SymbolicData f
  , forall c. Old.HasRep f (CompatContext c) => Old.RepData f (CompatContext c)
  ) => SymbolicData (CompatData f) where
  type Layout (CompatData f) c = Old.Layout f (Order c) :*: Old.Payload f (Order c)
  type HasRep (CompatData f) c = Old.HasRep f (CompatContext c)
  arithmetize (CompatData f) = compatContext (Old.arithmetize f) :*: Old.payload f
  interpolate c bs =
    CompatData $ Old.interpolate (bimap fromConstant compatData <$> bs)
               $ CompatContext (Par1 c)
  restore (layout :*: payload) = CompatData $ Old.restore (CompatContext layout, payload)

newtype CompatContext c f = CompatContext { compatContext :: f c }
  deriving NFData

instance NFData c => HNFData (CompatContext c) where
  hliftRnf liftRnf (CompatContext f) = liftRnf rnf f

instance HFunctor (CompatContext c) where
  hmap f = CompatContext . f . compatContext

instance HApplicative (CompatContext c) where
  hpure = CompatContext
  hliftA2 f (CompatContext x) (CompatContext y) = CompatContext (f x y)

instance Package (CompatContext c) where
  packWith f = CompatContext . f . fmap compatContext
  unpackWith f = fmap CompatContext . f . compatContext

instance Symbolic c => Old.Symbolic (CompatContext c) where
  type BaseField (CompatContext c) = Zp (Order c)
  type WitnessField (CompatContext c) = c
  witnessF = compatContext
  fromCircuitF (CompatContext x) f = CompatContext $ collect (f x)
    where
      collect :: Functor f => State [Constraint c] (f c) -> f c
      collect fs =
        let (xs, cs) = runState fs [] in flip (foldr constrain) cs <$> xs

instance
  (Symbolic c, Order c ~ n)
  => MonadCircuit c (Zp n) c (State [Constraint c]) where
  unconstrained = pure
  constraint f = modify' (Polynomial (compatAlgebra $ f fromConstant) :)
  lookupConstraint xs lt = modify' (Lookup lt xs :)

newtype CompatAlgebra a = CompatAlgebra { compatAlgebra :: a }
  deriving ( Functor, Zero, AdditiveSemigroup, AdditiveGroup
           , MultiplicativeSemigroup, MultiplicativeMonoid)

deriving instance {-# OVERLAPPABLE #-}
  FromConstant b a => FromConstant b (CompatAlgebra a)

deriving instance {-# OVERLAPPABLE #-} Scale b a => Scale b (CompatAlgebra a)

deriving instance AdditiveMonoid a => AdditiveMonoid (CompatAlgebra a)

deriving instance Semiring a => Semiring (CompatAlgebra a)

deriving instance Ring a => Ring (CompatAlgebra a)

deriving instance Finite a => Finite (CompatAlgebra a)

instance {-# OVERLAPPING #-} FromConstant (CompatAlgebra a) (CompatAlgebra a)

instance {-# OVERLAPPING #-}
  MultiplicativeSemigroup a => Scale (CompatAlgebra a) (CompatAlgebra a)

instance Exponent a b => Exponent (CompatAlgebra a) b where
  CompatAlgebra x ^ p = CompatAlgebra (x ^ p)

instance {-# OVERLAPPING #-}
  Scale Natural a => Scale (Zp n) (CompatAlgebra a) where
  scale = scale . toConstant

instance {-# OVERLAPPING #-}
  FromConstant Natural a => FromConstant (Zp n) (CompatAlgebra a) where
  fromConstant = fromConstant . toConstant

----------------------------------- ORPHANS ------------------------------------

instance {-# OVERLAPPABLE #-}
  (FromConstant Natural a, Order a ~ n) => FromConstant (Zp n) a where
  fromConstant = fromConstant . toConstant

instance {-# OVERLAPPABLE #-}
  (Scale Natural a, Order a ~ n) => Scale (Zp n) a where
  scale = scale . toConstant

instance {-# OVERLAPPABLE #-} PrimeField a => Witness a a where
  at x = x
