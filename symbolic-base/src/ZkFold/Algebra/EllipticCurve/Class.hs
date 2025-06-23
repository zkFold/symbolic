{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module ZkFold.Algebra.EllipticCurve.Class (
  -- * curve classes
  EllipticCurve (..),
  CyclicGroup (..),
  CycleOfCurves,
  WeierstrassCurve (..),
  TwistedEdwardsCurve (..),
  Compressible (..),
  Pairing (..),

  -- * point classes
  Planar (..),
  HasPointInf (..),

  -- * point types
  Weierstrass (..),
  TwistedEdwards (..),
  Point (..),
  JacobianPoint (..),
  CompressedPoint (..),
  AffinePoint (..),

  -- * point projections
  Project (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Data.String (fromString)
import GHC.Generics
import GHC.TypeLits (Symbol)
import Test.QuickCheck hiding (scale)
import Prelude (Integer, fromInteger, return, ($), (<$>), (>>=), type (~))
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Conditional
import ZkFold.Symbolic.Data.Eq
import ZkFold.Symbolic.Data.Input

-- | Elliptic curves are plane algebraic curves that form `AdditiveGroup`s.
-- Elliptic curves always have genus @1@ and are birationally equivalent
-- to a projective curve of degree @3@. As such, elliptic curves are
-- the simplest curves after conic sections, curves of degree @2@,
-- and lines, curves of degree @1@. Bézout's theorem implies
-- that a line in general position will intersect with an
-- elliptic curve at 3 points counting multiplicity;
-- @point0@, @point1@ and @point2@.
-- The geometric group law of the elliptic curve is:
--
-- > point0 + point1 + point2 = zero
class
  ( AdditiveGroup point
  , Eq (BaseFieldOf point)
  , Field (BaseFieldOf point)
  , Planar (BaseFieldOf point) point
  ) =>
  EllipticCurve point
  where
  type CurveOf point :: Symbol
  type BaseFieldOf point :: Type

  -- | `isOnCurve` validates an equation for a plane algebraic curve
  --   which has degree 3 up to some birational equivalence.
  isOnCurve :: point -> BooleanOf (BaseFieldOf point)

-- | Both the ECDSA and ECDH algorithms make use of
-- the elliptic curve discrete logarithm problem, ECDLP.
-- There may be a discrete "exponential" function
-- from a `PrimeField` of scalars
-- into the `AdditiveGroup` of points on an elliptic curve.
-- It's given naturally by scaling a point of prime order,
-- if there is one on the curve.
--
-- prop> scale order pointGen = zero
--
-- >>> let discreteExp scalar = scale scalar pointGen
--
-- Then the inverse of `discreteExp` is hard to compute.
class
  ( AdditiveGroup g
  , FiniteField (ScalarFieldOf g)
  , Scale (ScalarFieldOf g) g
  ) =>
  CyclicGroup g
  where
  type ScalarFieldOf g :: Type

  -- | generator of a cyclic subgroup
  --
  --   prop> scale (order @(ScalarFieldOf g)) pointGen = zero
  pointGen :: g

-- |
-- A cycle of two curves elliptic curves over finite fields
-- such that the number of points on one curve is equal to
-- the size of the field of definition of the next, in a cyclic way.
type CycleOfCurves g1 g2 =
  ( EllipticCurve g1
  , EllipticCurve g2
  , CyclicGroup g1
  , CyclicGroup g2
  , ScalarFieldOf g1 ~ BaseFieldOf g2
  , BaseFieldOf g1 ~ ScalarFieldOf g2
  )

-- | The standard form of an elliptic curve is the Weierstrass equation:
--
-- > y^2 = x^3 + a*x + b
--
-- * Weierstrass curves have x-axis symmetry.
-- * The characteristic of the field must not be @2@ or @3@.
-- * The curve must have nonzero discriminant @Δ = -16 * (4*a^3 + 27*b^3)@.
-- * When @a = 0@ some computations can be simplified so all the public
--   Weierstrass curves have @a = zero@ and nonzero @b@ and we do too.
class Field field => WeierstrassCurve (curve :: Symbol) field where
  weierstrassB :: field

-- | A twisted Edwards curve is defined by the equation:
--
-- > a*x^2 + y^2 = 1 + d*x^2*y^2
--
-- * Twisted Edwards curves have y-axis symmetry.
-- * The characteristic of the field must not be @2@.
-- * @a@ and @d@ must be nonzero.
class Field field => TwistedEdwardsCurve (curve :: Symbol) field where
  twistedEdwardsA :: field
  twistedEdwardsD :: field

class Eq (BaseFieldOf point) => Compressible point where
  type Compressed point :: Type
  pointCompressed
    :: BaseFieldOf point
    -> BooleanOf (BaseFieldOf point)
    -> Compressed point
  compress :: point -> Compressed point
  decompress :: Compressed point -> point

class
  ( CyclicGroup g1
  , CyclicGroup g2
  , Exponent gt (ScalarFieldOf g1)
  , MultiplicativeGroup gt
  , ScalarFieldOf g1 ~ ScalarFieldOf g2
  ) =>
  Pairing g1 g2 gt
    | g1 g2 -> gt
  where
  pairing :: g1 -> g2 -> gt

-- | A class for smart constructor method
-- `pointXY` for constructing points from an @x@ and @y@ coordinate.
class Planar field point | point -> field where
  pointXY :: field -> field -> point

-- | A class for smart constructor method
-- `pointInf` for constructing the point at infinity.
class HasPointInf point where pointInf :: point

-- | `Weierstrass` tags a `ProjectivePlanar` @point@, over a `Field` @field@,
-- with a phantom `WeierstrassCurve` @curve@.
newtype Weierstrass curve point = Weierstrass {pointWeierstrass :: point}
  deriving Generic

deriving anyclass instance NFData point => NFData (Weierstrass curve point)

deriving newtype instance
  Prelude.Eq point
  => Prelude.Eq (Weierstrass curve point)

deriving newtype instance
  Prelude.Show point
  => Prelude.Show (Weierstrass curve point)

deriving anyclass instance (BooleanOf point ~ Prelude.Bool, ToJSON point) => ToJSON (Weierstrass curve point)

deriving anyclass instance (BooleanOf point ~ Prelude.Bool, FromJSON point) => FromJSON (Weierstrass curve point)

instance
  ( Arbitrary (ScalarFieldOf (Weierstrass curve (Point field)))
  , CyclicGroup (Weierstrass curve (Point field))
  )
  => Arbitrary (Weierstrass curve (Point field))
  where
  arbitrary = do
    c <- arbitrary @(ScalarFieldOf (Weierstrass curve (Point field)))
    return $ scale c pointGen

instance
  ( Arbitrary (Weierstrass curve (Point field))
  , Conditional (BooleanOf field) (BooleanOf field)
  , Eq field
  , Field field
  )
  => Arbitrary (Weierstrass curve (JacobianPoint field))
  where
  arbitrary = project <$> arbitrary @(Weierstrass curve (Point field))

instance
  ( Arbitrary (ScalarFieldOf (Weierstrass curve (Point field)))
  , Compressed (Weierstrass curve (Point field))
      ~ Weierstrass curve (CompressedPoint field)
  , Compressible (Weierstrass curve (Point field))
  , CyclicGroup (Weierstrass curve (Point field))
  )
  => Arbitrary (Weierstrass curve (CompressedPoint field))
  where
  arbitrary = do
    c <- arbitrary @(Weierstrass curve (Point field))
    return $ compress c

instance
  ( Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  , WeierstrassCurve curve field
  )
  => EllipticCurve (Weierstrass curve (Point field))
  where
  type CurveOf (Weierstrass curve (Point field)) = curve
  type BaseFieldOf (Weierstrass curve (Point field)) = field
  isOnCurve (Weierstrass (Point x y isInf)) =
    if isInf
      then x == zero
      else
        let b = weierstrassB @curve in y * y == x * x * x + b

instance
  ( Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , EllipticCurve (Weierstrass curve (Point field))
  , Eq field
  , Field field
  , WeierstrassCurve curve field
  )
  => EllipticCurve (Weierstrass curve (JacobianPoint field))
  where
  type CurveOf (Weierstrass curve (JacobianPoint field)) = curve
  type BaseFieldOf (Weierstrass curve (JacobianPoint field)) = field
  isOnCurve p = isOnCurve (project p :: Weierstrass curve (Point field))

deriving newtype instance
  SymbolicEq field
  => SymbolicData (Weierstrass curve (Point field))

deriving newtype instance
  SymbolicEq field
  => SymbolicData (Weierstrass curve (JacobianPoint field))

instance
  ( SymbolicEq field
  , WeierstrassCurve curve field
  )
  => SymbolicInput (Weierstrass curve (Point field))
  where
  isValid = isOnCurve

instance
  ( SymbolicEq field
  , WeierstrassCurve curve field
  )
  => SymbolicInput (Weierstrass curve (JacobianPoint field))
  where
  isValid = isOnCurve

deriving newtype instance
  Conditional bool point
  => Conditional bool (Weierstrass curve point)

deriving newtype instance
  Eq point
  => Eq (Weierstrass curve point)

deriving newtype instance
  HasPointInf point
  => HasPointInf (Weierstrass curve point)

deriving newtype instance
  Planar field point
  => Planar field (Weierstrass curve point)

instance
  ( Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  )
  => AdditiveSemigroup (Weierstrass curve (Point field))
  where
  pt0@(Weierstrass (Point x0 y0 isInf0)) + pt1@(Weierstrass (Point x1 y1 isInf1)) =
    if isInf0
      then pt1
      else
        if isInf1
          then pt0 -- additive identity
          else
            if x0 == x1 && y0 + y1 == zero
              then pointInf -- additive inverse
              else
                let slope =
                      if x0 == x1 && y0 == y1
                        then (x0 * x0 + x0 * x0 + x0 * x0) // (y0 + y0) -- tangent
                        else (y1 - y0) // (x1 - x0) -- secant
                    x2 = slope * slope - x0 - x1
                    y2 = slope * (x0 - x2) - y0
                 in pointXY x2 y2

instance
  ( Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  )
  => AdditiveSemigroup (Weierstrass curve (JacobianPoint field))
  where
  -- https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-3.html#addition-add-2007-bl
  --
  pt1@(Weierstrass (JacobianPoint x1 y1 z1)) + pt2@(Weierstrass (JacobianPoint x2 y2 z2)) =
    if z1 == zero
      then pt2
      else
        if z2 == zero
          then pt1 -- additive identity
          else
            let !z1z1 = square z1
                !z2z2 = square z2
                !u1 = x1 * z2z2
                !u2 = x2 * z1z1
                !s1 = y1 * z2 * z2z2
                !s2 = y2 * z1 * z1z1
                !h = u2 - u1
                !i = square (double h)
                !j = h * i
                !r = double (s2 - s1)
                !v = u1 * i
                !x3 = square r - j - double v
                !y3 = r * (v - x3) - double s1 * j
                !z3 = (square (z1 + z2) - z1z1 - z2z2) * h
             in Weierstrass (JacobianPoint x3 y3 z3)

  -- https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-3.html#doubling-dbl-2007-bl
  --
  double pt@(Weierstrass (JacobianPoint x1 y1 z1)) =
    if z1 == zero
      then pt
      else
        let !xx = square x1
            !yy = square y1
            !yyyy = square yy
            !zz = square z1
            !s = double (square (x1 + yy) - xx - yyyy)
            !m = scale (3 :: Natural) xx
            !t = square m - double s
            !x3 = t
            !y3 = m * (s - t) - scale (8 :: Natural) yyyy
            !z3 = square (y1 + z1) - yy - zz
         in Weierstrass (JacobianPoint x3 y3 z3)

instance
  ( Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  , WeierstrassCurve curve field
  )
  => AdditiveMonoid (Weierstrass curve (Point field))
  where
  zero = pointInf

instance
  ( Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  , WeierstrassCurve curve field
  )
  => AdditiveMonoid (Weierstrass curve (JacobianPoint field))
  where
  zero = pointInf

instance
  ( Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  , WeierstrassCurve curve field
  )
  => AdditiveGroup (Weierstrass curve (Point field))
  where
  negate pt@(Weierstrass (Point x y isInf)) =
    if isInf then pt else pointXY x (negate y)

instance
  ( Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  , WeierstrassCurve curve field
  )
  => AdditiveGroup (Weierstrass curve (JacobianPoint field))
  where
  negate (Weierstrass (JacobianPoint x y z)) = Weierstrass (JacobianPoint x (negate y) z)

instance
  ( Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  , WeierstrassCurve curve field
  )
  => Scale Natural (Weierstrass curve (Point field))
  where
  scale = natScale

instance
  ( Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  , WeierstrassCurve curve field
  )
  => Scale Natural (Weierstrass curve (JacobianPoint field))
  where
  scale = natScale

instance
  ( Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  , WeierstrassCurve curve field
  )
  => Scale Integer (Weierstrass curve (Point field))
  where
  scale = intScale

instance
  ( Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  , WeierstrassCurve curve field
  )
  => Scale Integer (Weierstrass curve (JacobianPoint field))
  where
  scale = intScale

-- | `TwistedEdwards` tags a `Planar` @point@, over a `Field` @field@,
-- with a phantom `TwistedEdwardsCurve` @curve@.
newtype TwistedEdwards curve point = TwistedEdwards {pointTwistedEdwards :: point}
  deriving Generic

deriving anyclass instance NFData point => NFData (TwistedEdwards curve point)

instance
  ( Eq field
  , Field field
  , TwistedEdwardsCurve curve field
  )
  => EllipticCurve (TwistedEdwards curve (AffinePoint field))
  where
  type CurveOf (TwistedEdwards curve (AffinePoint field)) = curve
  type BaseFieldOf (TwistedEdwards curve (AffinePoint field)) = field
  isOnCurve (TwistedEdwards (AffinePoint x y)) =
    let a = twistedEdwardsA @curve
        d = twistedEdwardsD @curve
     in a * x * x + y * y == one + d * x * x * y * y

deriving newtype instance
  Prelude.Eq point
  => Prelude.Eq (TwistedEdwards curve point)

deriving newtype instance
  Prelude.Show point
  => Prelude.Show (TwistedEdwards curve point)

deriving newtype instance
  SymbolicOutput field
  => SymbolicData (TwistedEdwards curve (AffinePoint field))

instance
  ( Context field ~ ctx
  , Symbolic ctx
  , SymbolicEq field
  , TwistedEdwardsCurve curve field
  )
  => SymbolicInput (TwistedEdwards curve (AffinePoint field))
  where
  isValid = isOnCurve

deriving newtype instance
  Conditional bool point
  => Conditional bool (TwistedEdwards curve point)

deriving newtype instance
  Eq point
  => Eq (TwistedEdwards curve point)

deriving newtype instance
  HasPointInf point
  => HasPointInf (TwistedEdwards curve point)

deriving newtype instance
  Planar field point
  => Planar field (TwistedEdwards curve point)

instance
  ( Field field
  , TwistedEdwardsCurve curve field
  )
  => AdditiveSemigroup (TwistedEdwards curve (AffinePoint field))
  where
  TwistedEdwards (AffinePoint x0 y0) + TwistedEdwards (AffinePoint x1 y1) =
    let a = twistedEdwardsA @curve
        d = twistedEdwardsD @curve
        x2 = (x0 * y1 + y0 * x1) // (one + d * x0 * x1 * y0 * y1)
        y2 = (y0 * y1 - a * x0 * x1) // (one - d * x0 * x1 * y0 * y1)
     in pointXY x2 y2

instance
  ( Field field
  , TwistedEdwardsCurve curve field
  )
  => AdditiveMonoid (TwistedEdwards curve (AffinePoint field))
  where
  zero = pointXY zero one

instance
  ( Field field
  , TwistedEdwardsCurve curve field
  )
  => AdditiveGroup (TwistedEdwards curve (AffinePoint field))
  where
  negate (TwistedEdwards (AffinePoint x y)) = pointXY (negate x) y

instance
  ( Field field
  , TwistedEdwardsCurve curve field
  )
  => Scale Natural (TwistedEdwards curve (AffinePoint field))
  where
  scale = natScale

instance
  ( Field field
  , TwistedEdwardsCurve curve field
  )
  => Scale Integer (TwistedEdwards curve (AffinePoint field))
  where
  scale = intScale

instance
  ( Arbitrary (ScalarFieldOf (TwistedEdwards curve (AffinePoint field)))
  , CyclicGroup (TwistedEdwards curve (AffinePoint field))
  )
  => Arbitrary (TwistedEdwards curve (AffinePoint field))
  where
  arbitrary = do
    c <- arbitrary @(ScalarFieldOf (TwistedEdwards curve (AffinePoint field)))
    return $ scale c pointGen

-- | A type of points in the projective plane.
data Point field = Point
  { _x :: field
  , _y :: field
  , _zBit :: BooleanOf field
  }
  deriving Generic

deriving instance (NFData (BooleanOf field), NFData field) => NFData (Point field)

deriving instance
  (Prelude.Eq (BooleanOf field), Prelude.Eq field)
  => Prelude.Eq (Point field)

instance
  (BooleanOf field ~ Prelude.Bool, Prelude.Show field)
  => Prelude.Show (Point field)
  where
  show (Point x y isInf) =
    if isInf
      then "pointInf"
      else
        Prelude.mconcat
          ["(", Prelude.show x, ", ", Prelude.show y, ")"]

deriving instance (BooleanOf field ~ Prelude.Bool, ToJSON field) => ToJSON (Point field)

deriving instance (BooleanOf field ~ Prelude.Bool, FromJSON field) => FromJSON (Point field)

instance
  ( Context field ~ Context (BooleanOf field)
  , SymbolicOutput (BooleanOf field)
  , SymbolicOutput field
  )
  => SymbolicData (Point field)

instance Eq field => Planar field (Point field) where
  pointXY x y = Point x y false

instance (Eq field, Semiring field) => HasPointInf (Point field) where
  pointInf = Point zero one true

instance
  ( Conditional bool bool
  , Conditional bool field
  , bool ~ BooleanOf field
  )
  => Conditional bool (Point field)

instance
  ( BooleanOf (BooleanOf field) ~ BooleanOf field
  , Eq (BooleanOf field)
  , Eq field
  , Field field
  )
  => Eq (Point field)
  where
  Point x0 y0 isInf0 == Point x1 y1 isInf1 =
    if not isInf0 && not isInf1
      then x0 == x1 && y0 == y1
      else isInf0 && isInf1 && x1 * y0 == x0 * y1 -- same slope y0//x0 = y1//x1
  pt0 /= pt1 = not (pt0 == pt1)

data JacobianPoint field = JacobianPoint
  { _x :: field
  , _y :: field
  , _z :: field
  }
  deriving Generic

deriving instance NFData field => NFData (JacobianPoint field)

instance (Field field, Prelude.Eq field) => Prelude.Eq (JacobianPoint field) where
  -- If z0 /= 0 and z1 /= 0,
  -- x0 / z0^2 == x1 / z1^2 && y0 / z0^3 == y1 / z1^3
  JacobianPoint x0 y0 z0 == JacobianPoint x1 y1 z1 = x0 * z12 Prelude.== x1 * z02 && y0 * z13 Prelude.== y1 * z03
   where
    z12 = square z1
    z13 = z1 * z12
    z02 = square z0
    z03 = z0 * z02
  pt0 /= pt1 = not (pt0 Prelude.== pt1)

instance
  ( Context field ~ Context (BooleanOf field)
  , SymbolicOutput field
  )
  => SymbolicData (JacobianPoint field)

instance (Eq field, Field field) => Planar field (JacobianPoint field) where
  pointXY x y = JacobianPoint x y one

instance (Eq field, Semiring field) => HasPointInf (JacobianPoint field) where
  pointInf = JacobianPoint one one zero

instance
  (BooleanOf field ~ Prelude.Bool, Eq field, Field field, Prelude.Show field)
  => Prelude.Show (JacobianPoint field)
  where
  show (JacobianPoint x y z) =
    if z == zero
      then "pointInf"
      else
        Prelude.mconcat
          ["(", Prelude.show x, ", ", Prelude.show y, ", ", Prelude.show z, ")"]

instance
  ( Conditional bool bool
  , Conditional bool field
  , bool ~ BooleanOf field
  )
  => Conditional bool (JacobianPoint field)

instance
  ( BooleanOf (BooleanOf field) ~ BooleanOf field
  , Eq (BooleanOf field)
  , Eq field
  , Field field
  )
  => Eq (JacobianPoint field)
  where
  -- If z0 /= 0 and z1 /= 0,
  -- x0 / z0^2 == x1 / z1^2 && y0 / z0^3 == y1 / z1^3
  JacobianPoint x0 y0 z0 == JacobianPoint x1 y1 z1 = x0 * z12 == x1 * z02 && y0 * z13 == y1 * z03
   where
    z12 = square z1
    z13 = z1 * z12
    z02 = square z0
    z03 = z0 * z02
  pt0 /= pt1 = not (pt0 == pt1)

class Project a b where
  project :: a -> b

instance
  ( Conditional (BooleanOf field) (BooleanOf field)
  , Eq field
  , Field field
  )
  => Project (Point field) (JacobianPoint field)
  where
  project (Point x y isInf) =
    if isInf then pointInf else pointXY x y

instance
  ( Conditional (BooleanOf field) (BooleanOf field)
  , Eq field
  , Field field
  )
  => Project (JacobianPoint field) (Point field)
  where
  project (JacobianPoint x y z) =
    if z == zero
      then pointInf
      else
        let zz = square z
         in (pointXY (x // zz) (y // (z * zz)))

instance Project p1 p2 => Project (Weierstrass curve p1) (Weierstrass curve p2) where
  project (Weierstrass p1) = Weierstrass $ project p1

data CompressedPoint field = CompressedPoint
  { _x :: field
  , _yBit :: BooleanOf field
  , _zBit :: BooleanOf field
  }
  deriving Generic

deriving instance (NFData (BooleanOf field), NFData field) => NFData (CompressedPoint field)

deriving instance
  (Prelude.Show (BooleanOf field), Prelude.Show field)
  => Prelude.Show (CompressedPoint field)

deriving instance
  (Prelude.Eq (BooleanOf field), Prelude.Eq field)
  => Prelude.Eq (CompressedPoint field)

instance
  ( Context field ~ Context (BooleanOf field)
  , SymbolicOutput (BooleanOf field)
  , SymbolicOutput field
  )
  => SymbolicData (CompressedPoint field)

instance
  (AdditiveMonoid field, BoolType (BooleanOf field))
  => HasPointInf (CompressedPoint field)
  where
  pointInf = CompressedPoint zero true true

data AffinePoint field = AffinePoint
  { _x :: field
  , _y :: field
  }
  deriving (Generic, Prelude.Eq)

deriving instance NFData field => NFData (AffinePoint field)

instance SymbolicOutput field => SymbolicData (AffinePoint field)

instance Planar field (AffinePoint field) where pointXY = AffinePoint

instance Conditional bool field => Conditional bool (AffinePoint field)

instance
  ( Eq field
  , Field field
  )
  => Eq (AffinePoint field)

instance Prelude.Show field => Prelude.Show (AffinePoint field) where
  show (AffinePoint x y) =
    Prelude.mconcat
      ["(", Prelude.show x, ", ", Prelude.show y, ")"]
