{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module ZkFold.Algebra.EllipticCurve.Class
  ( -- * curve classes
    EllipticCurve (..)
  , CyclicGroup (..)
  , CycleOfCurves
  , WeierstrassCurve (..)
  , TwistedEdwardsCurve (..)
  , Compressible (..)
  , Pairing (..)
    -- * point classes
  , Planar (..)
  , HasPointInf (..)
    -- * point types
  , Weierstrass (..)
  , TwistedEdwards (..)
  , Point (..)
  , CompressedPoint (..)
  , AffinePoint (..)
  ) where

import           Control.DeepSeq                  (NFData)
import           Data.Kind                        (Type)
import           Data.String                      (fromString)
import           GHC.Generics
import           GHC.TypeLits                     (Symbol)
import           Prelude                          (Integer, return, type (~), ($), (>>=))
import qualified Prelude
import           Test.QuickCheck                  hiding (scale)

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Number
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.Input

{- | Elliptic curves are plane algebraic curves that form `AdditiveGroup`s.
Elliptic curves always have genus @1@ and are birationally equivalent
to a projective curve of degree @3@. As such, elliptic curves are
the simplest curves after conic sections, curves of degree @2@,
and lines, curves of degree @1@. Bézout's theorem implies
that a line in general position will intersect with an
elliptic curve at 3 points counting multiplicity;
@point0@, @point1@ and @point2@.
The geometric group law of the elliptic curve is:

> point0 + point1 + point2 = zero
-}
class
  ( Field (BaseFieldOf point)
  , Eq (BaseFieldOf point)
  , Planar (BaseFieldOf point) point
  , AdditiveGroup point
  ) => EllipticCurve point where
    type CurveOf point :: Symbol
    type BaseFieldOf point :: Type
    -- | `isOnCurve` validates an equation for a plane algebraic curve
    -- which has degree 3 up to some birational equivalence.
    isOnCurve :: point -> BooleanOf (BaseFieldOf point)

{- | Both the ECDSA and ECDH algorithms make use of
the elliptic curve discrete logarithm problem, ECDLP.
There may be a discrete "exponential" function
from a `PrimeField` of scalars
into the `AdditiveGroup` of points on an elliptic curve.
It's given naturally by scaling a point of prime order,
if there is one on the curve.

prop> scale order pointGen = zero

>>> let discreteExp scalar = scale scalar pointGen

Then the inverse of `discreteExp` is hard to compute.
-}
class
  ( AdditiveGroup g
  , FiniteField (ScalarFieldOf g)
  , Scale (ScalarFieldOf g) g
  ) => CyclicGroup g where
    type ScalarFieldOf g :: Type
    -- | generator of a cyclic subgroup
    --
    -- prop> scale (order @(ScalarFieldOf g)) pointGen = zero
    pointGen :: g

{- |
A cycle of two curves elliptic curves over finite fields
such that the number of points on one curve is equal to
the size of the field of definition of the next, in a cyclic way.
-}
type CycleOfCurves g1 g2 =
  ( EllipticCurve g1
  , EllipticCurve g2
  , CyclicGroup g1
  , CyclicGroup g2
  , ScalarFieldOf g1 ~ BaseFieldOf g2
  , BaseFieldOf g1 ~ ScalarFieldOf g2
  )

{- | The standard form of an elliptic curve is the Weierstrass equation:

> y^2 = x^3 + a*x + b

* Weierstrass curves have x-axis symmetry.
* The characteristic of the field must not be @2@ or @3@.
* The curve must have nonzero discriminant @Δ = -16 * (4*a^3 + 27*b^3)@.
* When @a = 0@ some computations can be simplified so all the public
  Weierstrass curves have @a = zero@ and nonzero @b@ and we do too.
-}
class Field field => WeierstrassCurve (curve :: Symbol) field where
  weierstrassB :: field

{- | A twisted Edwards curve is defined by the equation:

> a*x^2 + y^2 = 1 + d*x^2*y^2

* Twisted Edwards curves have y-axis symmetry.
* The characteristic of the field must not be @2@.
* @a@ and @d@ must be nonzero.
-}
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
  , ScalarFieldOf g1 ~ ScalarFieldOf g2
  , MultiplicativeGroup gt
  , Exponent gt (ScalarFieldOf g1)
  ) => Pairing g1 g2 gt | g1 g2 -> gt where
    pairing :: g1 -> g2 -> gt

{- | A class for smart constructor method
`pointXY` for constructing points from an @x@ and @y@ coordinate. -}
class Planar field point | point -> field where
  pointXY :: field -> field -> point

{- | A class for smart constructor method
`pointInf` for constructing the point at infinity. -}
class HasPointInf point where pointInf :: point

{- | `Weierstrass` tags a `ProjectivePlanar` @point@, over a `Field` @field@,
with a phantom `WeierstrassCurve` @curve@. -}
newtype Weierstrass curve point = Weierstrass {pointWeierstrass :: point}
  deriving Generic
deriving anyclass instance NFData point => NFData (Weierstrass curve point)
deriving newtype instance Prelude.Eq point
  => Prelude.Eq (Weierstrass curve point)
deriving newtype instance Prelude.Show point
  => Prelude.Show (Weierstrass curve point)
instance
  ( Arbitrary (ScalarFieldOf (Weierstrass curve (Point field)))
  , CyclicGroup (Weierstrass curve (Point field))
  ) => Arbitrary (Weierstrass curve (Point field)) where
    arbitrary = do
      c <- arbitrary @(ScalarFieldOf (Weierstrass curve (Point field)))
      return $ scale c pointGen
instance
  ( Arbitrary (ScalarFieldOf (Weierstrass curve (Point field)))
  , CyclicGroup (Weierstrass curve (Point field))
  , Compressible (Weierstrass curve (Point field))
  , Compressed (Weierstrass curve (Point field))
    ~ Weierstrass curve (CompressedPoint field)
  ) => Arbitrary (Weierstrass curve (CompressedPoint field)) where
    arbitrary = do
      c <- arbitrary @(Weierstrass curve (Point field))
      return $ compress c
instance
  ( WeierstrassCurve curve field
  , Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  ) => EllipticCurve (Weierstrass curve (Point field)) where
    type CurveOf (Weierstrass curve (Point field)) = curve
    type BaseFieldOf (Weierstrass curve (Point field)) = field
    isOnCurve (Weierstrass (Point x y isInf)) =
      if isInf then x == zero else
      let b = weierstrassB @curve in y*y == x*x*x + b
deriving newtype instance
  ( SymbolicEq field
  ) => SymbolicData (Weierstrass curve (Point field))
instance
  ( WeierstrassCurve curve field
  , SymbolicEq field
  ) => SymbolicInput (Weierstrass curve (Point field)) where
    isValid = isOnCurve
deriving newtype instance Conditional bool point
  => Conditional bool (Weierstrass curve point)
deriving newtype instance Eq point
  => Eq (Weierstrass curve point)
deriving newtype instance HasPointInf point
  => HasPointInf (Weierstrass curve point)
deriving newtype instance Planar field point
  => Planar field (Weierstrass curve point)
instance
  ( Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  ) => AdditiveSemigroup (Weierstrass curve (Point field)) where
    pt0@(Weierstrass (Point x0 y0 isInf0)) + pt1@(Weierstrass (Point x1 y1 isInf1)) =
      if isInf0 then pt1 else if isInf1 then pt0 -- additive identity
      else if x0 == x1 && y0 + y1 == zero then pointInf -- additive inverse
      else let !slope = if x0 == x1 && y0 == y1
                        then (x0 * x0 + x0 * x0 + x0 * x0) // (y0 + y0) -- tangent
                        else (y1 - y0) // (x1 - x0) -- secant
               !x2 = slope * slope - x0 - x1
               !y2 = slope * (x0 - x2) - y0
            in pointXY x2 y2
instance
  ( WeierstrassCurve curve field
  , Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  ) => AdditiveMonoid (Weierstrass curve (Point field)) where
    zero = pointInf
instance
  ( WeierstrassCurve curve field
  , Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  ) => AdditiveGroup (Weierstrass curve (Point field)) where
    negate pt@(Weierstrass (Point x y isInf)) =
      if isInf then pt else pointXY x (negate y)
instance
  ( WeierstrassCurve curve field
  , Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  ) => Scale Natural (Weierstrass curve (Point field)) where
  scale = natScale
instance
  ( WeierstrassCurve curve field
  , Conditional (BooleanOf field) (BooleanOf field)
  , Conditional (BooleanOf field) field
  , Eq field
  , Field field
  ) => Scale Integer (Weierstrass curve (Point field)) where
  scale = intScale

{- | `TwistedEdwards` tags a `Planar` @point@, over a `Field` @field@,
with a phantom `TwistedEdwardsCurve` @curve@. -}
newtype TwistedEdwards curve point = TwistedEdwards {pointTwistedEdwards :: point}
  deriving Generic
deriving anyclass instance NFData point => NFData (TwistedEdwards curve point)
instance
  ( TwistedEdwardsCurve curve field
  , Field field
  , Eq field
  ) => EllipticCurve (TwistedEdwards curve (AffinePoint field)) where
    type CurveOf (TwistedEdwards curve (AffinePoint field)) = curve
    type BaseFieldOf (TwistedEdwards curve (AffinePoint field)) = field
    isOnCurve (TwistedEdwards (AffinePoint x y)) =
      let !a = twistedEdwardsA @curve
          !d = twistedEdwardsD @curve
      in a*x*x + y*y == one + d*x*x*y*y
deriving newtype instance Prelude.Eq point
  => Prelude.Eq (TwistedEdwards curve point)
deriving newtype instance Prelude.Show point
  => Prelude.Show (TwistedEdwards curve point)
deriving newtype instance SymbolicOutput field
  => SymbolicData (TwistedEdwards curve (AffinePoint field))
instance
  ( Context field ~ ctx
  , Symbolic ctx
  , TwistedEdwardsCurve curve field
  , SymbolicEq field
  ) => SymbolicInput (TwistedEdwards curve (AffinePoint field)) where
    isValid = isOnCurve
deriving newtype instance Conditional bool point
  => Conditional bool (TwistedEdwards curve point)
deriving newtype instance Eq point
  => Eq (TwistedEdwards curve point)
deriving newtype instance HasPointInf point
  => HasPointInf (TwistedEdwards curve point)
deriving newtype instance Planar field point
  => Planar field (TwistedEdwards curve point)
instance
  ( TwistedEdwardsCurve curve field
  , Field field
  ) => AdditiveSemigroup (TwistedEdwards curve (AffinePoint field)) where
    TwistedEdwards (AffinePoint x0 y0) + TwistedEdwards (AffinePoint x1 y1) =
      let !a = twistedEdwardsA @curve
          !d = twistedEdwardsD @curve
          !x2 = (x0 * y1 + y0 * x1) // (one + d * x0 * x1 * y0 * y1)
          !y2 = (y0 * y1 - a * x0 * x1) // (one - d * x0 * x1 * y0 * y1)
      in pointXY x2 y2
instance
  ( TwistedEdwardsCurve curve field
  , Field field
  ) => AdditiveMonoid (TwistedEdwards curve (AffinePoint field)) where
    zero = pointXY zero one
instance
  ( TwistedEdwardsCurve curve field
  , Field field
  ) => AdditiveGroup (TwistedEdwards curve (AffinePoint field)) where
    negate (TwistedEdwards (AffinePoint x y)) = pointXY (negate x) y
instance
  ( TwistedEdwardsCurve curve field
  , Field field
  ) => Scale Natural (TwistedEdwards curve (AffinePoint field)) where
  scale = natScale
instance
  ( TwistedEdwardsCurve curve field
  , Field field
  ) => Scale Integer (TwistedEdwards curve (AffinePoint field)) where
  scale = intScale
instance
  ( Arbitrary (ScalarFieldOf (TwistedEdwards curve (AffinePoint field)))
  , CyclicGroup (TwistedEdwards curve (AffinePoint field))
  ) => Arbitrary (TwistedEdwards curve (AffinePoint field)) where
    arbitrary = do
      c <- arbitrary @(ScalarFieldOf (TwistedEdwards curve (AffinePoint field)))
      return $ scale c pointGen

{- | A type of points in the projective plane. -}
data Point field = Point
  { _x    :: field
  , _y    :: field
  , _zBit :: BooleanOf field
  } deriving (Generic)
deriving instance (NFData field, NFData (BooleanOf field)) => NFData (Point field)
deriving instance (Prelude.Eq (BooleanOf field), Prelude.Eq field)
  => Prelude.Eq (Point field)
instance
  ( SymbolicOutput (BooleanOf field)
  , SymbolicOutput field
  , Context field ~ Context (BooleanOf field)
  ) => SymbolicData (Point field)
instance Eq field => Planar field (Point field) where
  pointXY x y = Point x y false
instance (Semiring field, Eq field) => HasPointInf (Point field) where
  pointInf = Point zero one true
instance (Prelude.Show field, BooleanOf field ~ Prelude.Bool)
  => Prelude.Show (Point field) where
  show (Point x y isInf) =
    if isInf then "pointInf" else Prelude.mconcat
      ["(", Prelude.show x, ", ", Prelude.show y, ")"]
instance
  ( Conditional bool bool
  , Conditional bool field
  , bool ~ BooleanOf field
  ) => Conditional bool (Point field)
instance
  ( BooleanOf (BooleanOf field) ~ BooleanOf field
  , Eq (BooleanOf field)
  , Eq field
  , Field field
  ) => Eq (Point field) where
    Point x0 y0 isInf0 == Point x1 y1 isInf1 =
      if not isInf0 && not isInf1
      then x0 == x1 && y0 == y1
      else isInf0 && isInf1 && x1*y0 == x0*y1 -- same slope y0//x0 = y1//x1
    pt0 /= pt1 = not (pt0 == pt1)

data CompressedPoint field = CompressedPoint
  { _x    :: field
  , _yBit :: BooleanOf field
  , _zBit :: BooleanOf field
  } deriving Generic
deriving instance (NFData field, NFData (BooleanOf field)) => NFData (CompressedPoint field)
deriving instance (Prelude.Show (BooleanOf field), Prelude.Show field)
  => Prelude.Show (CompressedPoint field)
deriving instance (Prelude.Eq (BooleanOf field), Prelude.Eq field)
  => Prelude.Eq (CompressedPoint field)
instance
  ( SymbolicOutput (BooleanOf field)
  , SymbolicOutput field
  , Context field ~ Context (BooleanOf field)
  ) => SymbolicData (CompressedPoint field)
instance (BoolType (BooleanOf field), AdditiveMonoid field)
  => HasPointInf (CompressedPoint field) where
    pointInf = CompressedPoint zero true true

data AffinePoint field = AffinePoint
  { _x :: field
  , _y :: field
  } deriving (Generic, Prelude.Eq)
deriving instance NFData field => NFData (AffinePoint field)
instance SymbolicOutput field => SymbolicData (AffinePoint field)
instance Planar field (AffinePoint field) where pointXY = AffinePoint
instance Conditional bool field => Conditional bool (AffinePoint field)
instance
  ( Eq field
  , Field field
  ) => Eq (AffinePoint field)
instance Prelude.Show field => Prelude.Show (AffinePoint field) where
  show (AffinePoint x y) = Prelude.mconcat
    ["(", Prelude.show x, ", ", Prelude.show y, ")"]
