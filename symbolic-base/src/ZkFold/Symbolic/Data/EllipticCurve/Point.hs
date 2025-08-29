{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.Point where

import Control.DeepSeq (NFData)
import qualified Data.Eq as Haskell
import Data.Function (const, ($), (.))
import Data.Functor (Functor, fmap)
import Data.Tuple (curry, uncurry)
import Data.Type.Equality (type (~))
import GHC.Generics (Generic, Generic1, Par1 (..), U1 (..))

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class hiding (Point)
import qualified ZkFold.Algebra.EllipticCurve.Class as Elliptic
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq
import ZkFold.Data.HFunctor.Classes (HEq, HNFData)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class (SymbolicData)

data Point curve f c = Point
  { px :: f c
  , py :: f c
  , pIsInf :: Bool c
  }
  deriving (Generic, Generic1, SymbolicData)

deriving instance (HNFData c, NFData (f c)) => NFData (Point curve f c)

deriving instance (HEq c, Haskell.Eq (f c)) => Haskell.Eq (Point curve f c)

instance
  BooleanOf (f c) ~ Bool c
  => FromConstant (Elliptic.Point (f c)) (Point curve f c)
  where
  fromConstant Elliptic.Point {..} = Point {px = _x, py = _y, pIsInf = _zBit}

instance BooleanOf (f c) ~ Bool c => ToConstant (Point curve f c) where
  type Const (Point curve f c) = Elliptic.Point (f c)
  toConstant Point {..} = Elliptic.Point {_x = px, _y = py, _zBit = pIsInf}

instance
  (Symbolic c, SymbolicEq f c)
  => Conditional (Bool c) (Elliptic.Point (f c))
  where
  bool (fromConstant -> e) (fromConstant -> t) b =
    toConstant @(Point "" f c) $ bool e t b

data Two a = Two a a deriving Functor

unTwo :: Two a -> (a, a)
unTwo (Two x y) = (x, y)

type WEP c f d = Weierstrass c (Elliptic.Point (f d))

viaWeierstrass
  :: (w ~ WEP curve f c, p ~ Point curve f c, Functor g, BooleanOf (f c) ~ Bool c)
  => (g w -> w) -> g p -> p
viaWeierstrass f =
  fromConstant . pointWeierstrass . f . fmap (Weierstrass . toConstant)

instance Symbolic c => Planar (f c) (Point curve f c) where
  pointXY px py = Point {pIsInf = false, ..}

instance (Symbolic c, Semiring (f c)) => HasPointInf (Point curve f c) where
  pointInf = Point zero one true

instance
  (Symbolic c, MultiplicativeSemigroup (f c), SymbolicEq f c)
  => Eq (Point curve f c)
  where
  type BooleanOf (Point curve f c) = Bool c
  Point x y i == Point x' y' i' =
    ifThenElse (i || i') (i && i' && x' * y == x * y') ((x, y) == (x', y'))

instance
  (Symbolic c, SymbolicEq f c, Semiring (f c))
  => Zero (Point curve f c)
  where
  zero = viaWeierstrass (const zero) U1

instance
  {-# OVERLAPPABLE #-}
  (BooleanOf (f c) ~ Bool c, Scale k (WEP curve f c))
  => Scale k (Point curve f c)
  where
  scale k = viaWeierstrass (scale k . unPar1) . Par1

instance
  (Symbolic c, SymbolicEq f c, Field (f c))
  => AdditiveSemigroup (Point curve f c)
  where
  (+) = curry $ viaWeierstrass (uncurry (+) . unTwo) . uncurry Two

instance
  (Symbolic c, SymbolicEq f c, WeierstrassCurve curve (f c))
  => AdditiveMonoid (Point curve f c)

instance
  (Symbolic c, SymbolicEq f c, WeierstrassCurve curve (f c))
  => AdditiveGroup (Point curve f c)
  where
  negate = viaWeierstrass (negate . unPar1) . Par1
