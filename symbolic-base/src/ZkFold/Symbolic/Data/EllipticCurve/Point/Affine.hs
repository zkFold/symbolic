{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.EllipticCurve.Point.Affine where

import qualified ZkFold.Algebra.EllipticCurve.Class as Elliptic
import GHC.Generics (Generic1)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import Data.Coerce (coerce, Coercible)
import ZkFold.Algebra.Class
import Data.Function ((.))
import ZkFold.Algebra.EllipticCurve.Class (Planar (..))

newtype AffinePoint curve f c =
  AffinePoint { affinePoint :: Elliptic.AffinePoint (f c) }
  deriving stock (Generic1)
  deriving anyclass (SymbolicData)

type Base nt cv f c = nt cv (Elliptic.AffinePoint (f c))

type Pt nt cv f c = AffinePoint (nt cv) f c

instance
  (Planar field (Base nt cv f c), Coercible (Base nt cv f c) (Pt nt cv f c))
  => Planar field (Pt nt cv f c) where
  pointXY x y = coerce @(Base nt cv f c) (pointXY x y)

instance
  (Zero (Base nt cv f c), Coercible (Base nt cv f c) (Pt nt cv f c))
  => Zero (Pt nt cv f c) where
  zero = coerce @(Base nt cv f c) zero

instance
  (AdditiveSemigroup (Base nt cv f c), Coercible (Base nt cv f c) (Pt nt cv f c))
  => AdditiveSemigroup (Pt nt cv f c) where
  x + y = coerce @(Base nt cv f c) (coerce x + coerce y)

instance {-# OVERLAPPABLE #-}
  (Scale k (Base nt cv f c), Coercible (Base nt cv f c) (Pt nt cv f c))
  => Scale k (Pt nt cv f c) where
  scale k = coerce @(Base nt cv f c) . scale k . coerce

instance
  (AdditiveMonoid (Base nt cv f c), Coercible (Base nt cv f c) (Pt nt cv f c))
  => AdditiveMonoid (Pt nt cv f c)

instance
  (AdditiveGroup (Base nt cv f c), Coercible (Base nt cv f c) (Pt nt cv f c))
  => AdditiveGroup (Pt nt cv f c) where
  negate = coerce @(Base nt cv f c) . negate . coerce
