{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
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
import Data.Foldable (foldr)
import Data.Function (flip, ($), (.))
import Data.Functor (Functor, fmap, (<$>))
import Data.Type.Equality (type (~))
import GHC.Generics (Par1 (..), (:*:) (..))
import GHC.Stack (CallStack, callStack)
import Numeric.Natural (Natural)

import ZkFold.Algebra.Class
import ZkFold.Algebra.Field (Zp)
import ZkFold.Control.HApplicative (HApplicative (..))
import ZkFold.Data.HFunctor (HFunctor (..))
import ZkFold.Data.HFunctor.Classes (HNFData (..))
import ZkFold.Data.Package (Package (..))
import qualified ZkFold.Symbolic.Class as Old
import qualified ZkFold.Symbolic.Data.Class as Old
import ZkFold.Symbolic.Data.V2 (SymbolicData (..))
import ZkFold.Symbolic.MonadCircuit (MonadCircuit (..), Witness (..))
import ZkFold.Symbolic.V2 (Constraint (..), Symbolic, constrain)

newtype CompatData f c = CompatData {compatData :: f (CompatContext c)}

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

newtype CompatContext c f = CompatContext {compatContext :: f c}

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

instance Symbolic c => Old.Symbolic (CompatContext c) where
  type BaseField (CompatContext c) = Zp (Order c)
  type WitnessField (CompatContext c) = c
  witnessF = compatContext
  fromCircuitF (CompatContext x) f = CompatContext $ collect (f x)
   where
    collect :: Functor f => State [(CallStack, Constraint c)] (f c) -> f c
    collect fs = let (xs, cs) = runState fs [] in flip (foldr work) cs <$> xs
    work :: (CallStack, Constraint c) -> c -> c
    work (stack, constr) elem = let ?callStack = stack in constrain constr elem

instance
  (Symbolic c, Order c ~ n)
  => MonadCircuit c (Zp n) c (State [(CallStack, Constraint c)])
  where
  unconstrained = pure
  constraint f =
    let stack = callStack
     in modify' ((stack, Polynomial (compatAlgebra $ f fromConstant)) :)
  lookupConstraint xs lt =
    let stack = callStack in modify' ((stack, Lookup lt xs) :)

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

instance {-# OVERLAPPABLE #-} PrimeField a => Witness a a where
  at x = x
