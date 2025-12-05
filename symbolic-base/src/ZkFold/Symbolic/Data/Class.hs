{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Class where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Binary (Binary)
import Data.Constraint (Constraint)
import Data.Function (($), (.))
import Data.Functor (Functor, fmap, (<$>))
import Data.Functor.Rep (Rep, Representable, pureRep)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Semialign (Semialign)
import Data.Type.Equality (type (~))
import qualified GHC.Generics as G
import Numeric.Natural (Natural)

import ZkFold.Algebra.Class
import qualified ZkFold.Algorithm.Interpolation as I
import ZkFold.Data.Product (fstP, sndP)
import ZkFold.Symbolic.Boot (FieldElement (..))
import ZkFold.Symbolic.Class (Symbolic)

class Functor (Layout f c) => LayoutData f c

instance Functor (Layout f c) => LayoutData f c

class Layout f c ~ Layout f d => EquivData f c d

instance Layout f c ~ Layout f d => EquivData f c d

type RepFunctor f = (Representable f, Binary (Rep f))

class RepFunctor (Layout f c) => RepData f c

instance RepFunctor (Layout f c) => RepData f c

class
  ( forall c. LayoutData f c
  , forall c d. Order c ~ Order d => EquivData f c d
  , forall c. HasRep f c => RepData f c
  ) =>
  SymbolicData (f :: Type -> Type)
  where
  type Layout f (c :: Type) :: Type -> Type
  type Layout f c = Layout (G.Rep1 f) c

  type HasRep f (c :: Type) :: Constraint
  type HasRep f c = HasRep (G.Rep1 f) c

  toLayout :: Symbolic c => f c -> Layout f c c
  default toLayout
    :: (Symbolic c, G.Generic1 f, SymbolicData (G.Rep1 f))
    => Layout f c ~ Layout (G.Rep1 f) c
    => f c -> Layout f c c
  toLayout = toLayout . G.from1

  interpolate :: Symbolic c => c -> NonEmpty (Natural, f c) -> f c
  default interpolate
    :: (Symbolic c, G.Generic1 f, SymbolicData (G.Rep1 f))
    => c -> NonEmpty (Natural, f c) -> f c
  interpolate v = G.to1 . interpolate v . fmap (fmap G.from1)

  fromLayout :: Symbolic c => Layout f c c -> f c
  default fromLayout
    :: (Symbolic c, G.Generic1 f, SymbolicData (G.Rep1 f))
    => Layout f c ~ Layout (G.Rep1 f) c
    => Layout f c c -> f c
  fromLayout = G.to1 . fromLayout

dummy :: (SymbolicData f, HasRep f c, Symbolic c) => f c
dummy = fromLayout (pureRep zero)

instance SymbolicData G.U1 where
  type Layout G.U1 _ = G.U1
  type HasRep G.U1 _ = ()
  toLayout u = u
  interpolate _ _ = G.U1
  fromLayout u = u

instance SymbolicData G.Par1 where
  type Layout G.Par1 _ = G.Par1
  type HasRep G.Par1 _ = ()
  toLayout p = p
  interpolate c =
    G.Par1
      . fromFieldElement
      . I.interpolate (FieldElement c)
      . fmap (bimap fromConstant $ FieldElement . G.unPar1)
  fromLayout p = p

instance (SymbolicData f, SymbolicData g) => SymbolicData (f G.:*: g) where
  type Layout (f G.:*: g) c = Layout f c G.:*: Layout g c
  type HasRep (f G.:*: g) c = (HasRep f c, HasRep g c)
  toLayout (f G.:*: g) = toLayout f G.:*: toLayout g
  interpolate c bs =
    interpolate c (fmap fstP <$> bs) G.:*: interpolate c (fmap sndP <$> bs)
  fromLayout (f G.:*: g) = fromLayout f G.:*: fromLayout g

instance (Semialign f, SymbolicData g) => SymbolicData (f G.:.: g) where
  type Layout (f G.:.: g) c = f G.:.: Layout g c
  type HasRep (f G.:.: g) c = (RepFunctor f, HasRep g c)
  toLayout = G.Comp1 . fmap toLayout . G.unComp1
  interpolate c =
    G.Comp1 . fmap (interpolate c) . I.pushInterpolation . fmap (G.unComp1 <$>)
  fromLayout = G.Comp1 . fmap fromLayout . G.unComp1

instance SymbolicData f => SymbolicData (G.M1 i d f) where
  type Layout (G.M1 i d f) c = Layout f c
  type HasRep (G.M1 i d f) c = HasRep f c
  toLayout = toLayout . G.unM1
  interpolate c = G.M1 . interpolate c . fmap (G.unM1 <$>)
  fromLayout = G.M1 . fromLayout

instance SymbolicData f => SymbolicData (G.Rec1 f) where
  type Layout (G.Rec1 f) c = Layout f c
  type HasRep (G.Rec1 f) c = HasRep f c
  toLayout = toLayout . G.unRec1
  interpolate c = G.Rec1 . interpolate c . fmap (G.unRec1 <$>)
  fromLayout = G.Rec1 . fromLayout
