{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Data.V2 where

import Data.Kind (Type)
import Data.Type.Equality (type (~))
import qualified GHC.Generics as G
import Data.Constraint (Constraint)
import Data.Functor.Rep (Representable, Rep, pureRep)
import Data.Binary (Binary)
import ZkFold.Symbolic.V2 (Symbolic)
import Data.Function ((.))
import Data.List.NonEmpty (NonEmpty)
import Data.Functor (fmap, (<$>))
import ZkFold.Algebra.Class (zero)
import ZkFold.Data.Product (fstP, sndP)
import Data.Semialign (Semialign)
import Numeric.Natural (Natural)
import ZkFold.Symbolic.Algorithm.Interpolation (pushInterpolation)

type RepFunctor f = (Representable f, Binary (Rep f))

class RepFunctor (Layout f c) => RepData f c

instance RepFunctor (Layout f c) => RepData f c

class (forall c. HasRep f c => RepData f c) => SymbolicData (f :: Type -> Type) where
  type Layout f (c :: Type) :: Type -> Type
  type Layout f c = Layout (G.Rep1 f) c

  type HasRep f (c :: Type) :: Constraint
  type HasRep f c = HasRep (G.Rep1 f) c

  arithmetize :: Symbolic c => f c -> Layout f c c
  default arithmetize
    :: (Symbolic c, G.Generic1 f, SymbolicData (G.Rep1 f))
    => (Layout f c ~ Layout (G.Rep1 f) c)
    => f c -> Layout f c c
  arithmetize = arithmetize . G.from1

  interpolate :: Symbolic c => c -> NonEmpty (Natural, f c) -> f c
  default interpolate
    :: (Symbolic c, G.Generic1 f, SymbolicData (G.Rep1 f))
    => c -> NonEmpty (Natural, f c) -> f c
  interpolate v = G.to1 . interpolate v . fmap (fmap G.from1)

  restore :: Symbolic c => Layout f c c -> f c
  default restore
    :: (Symbolic c, G.Generic1 f, SymbolicData (G.Rep1 f))
    => (Layout f c ~ Layout (G.Rep1 f) c)
    => Layout f c c -> f c
  restore = G.to1 . restore

dummy :: (SymbolicData f, HasRep f c, Symbolic c) => f c
dummy = restore (pureRep zero)

instance SymbolicData G.U1 where
  type Layout G.U1 _ = G.U1
  type HasRep G.U1 _ = ()
  arithmetize u = u
  interpolate _ _ = G.U1
  restore u = u

instance (SymbolicData f, SymbolicData g) => SymbolicData (f G.:*: g) where
  type Layout (f G.:*: g) c = Layout f c G.:*: Layout g c
  type HasRep (f G.:*: g) c = (HasRep f c, HasRep g c)
  arithmetize (f G.:*: g) = arithmetize f G.:*: arithmetize g
  interpolate c bs =
    interpolate c (fmap fstP <$> bs) G.:*: interpolate c (fmap sndP <$> bs)
  restore (f G.:*: g) = restore f G.:*: restore g

instance (Semialign f, SymbolicData g) => SymbolicData (f G.:.: g) where
  type Layout (f G.:.: g) c = f G.:.: Layout g c
  type HasRep (f G.:.: g) c = (RepFunctor f, HasRep g c)
  arithmetize = G.Comp1 . fmap arithmetize . G.unComp1
  interpolate c =
    G.Comp1 . fmap (interpolate c) . pushInterpolation . fmap (G.unComp1 <$>)
  restore = G.Comp1 . fmap restore . G.unComp1

instance SymbolicData f => SymbolicData (G.M1 i d f) where
  type Layout (G.M1 i d f) c = Layout f c
  type HasRep (G.M1 i d f) c = HasRep f c
  arithmetize = arithmetize . G.unM1
  interpolate c = G.M1 . interpolate c . fmap (G.unM1 <$>)
  restore = G.M1 . restore

instance SymbolicData f => SymbolicData (G.Rec1 f) where
  type Layout (G.Rec1 f) c = Layout f c
  type HasRep (G.Rec1 f) c = HasRep f c
  arithmetize = arithmetize . G.unRec1
  interpolate c = G.Rec1 . interpolate c . fmap (G.unRec1 <$>)
  restore = G.Rec1 . restore
