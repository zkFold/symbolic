{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.FFI.Rust.RustBLS where

import Control.Monad
import qualified Data.Vector as V
import GHC.Base
import GHC.Natural (naturalFromInteger)
import Test.QuickCheck hiding (scale)
import Prelude hiding (fromIntegral, negate, sum, (*), (+), (-), (^))
import qualified Prelude as P

import ZkFold.Algebra.Class hiding (sum)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (
  BLS12_381_G1_CompressedPoint,
  BLS12_381_G1_Point,
  BLS12_381_GT,
  BLS12_381_Scalar,
 )
import qualified ZkFold.Algebra.EllipticCurve.BLS12_381 as EC
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Univariate
import ZkFold.Data.Binary
import qualified ZkFold.Data.Eq
import ZkFold.FFI.Rust.Conversion
import ZkFold.FFI.Rust.Poly ()
import ZkFold.FFI.Rust.Runner (runRustFun0, runRustFun1, runRustFun2, runRustFun3)
import ZkFold.FFI.Rust.RustFunctions
import ZkFold.FFI.Rust.Types
import ZkFold.Symbolic.MonadCircuit

deriveIntegerFromNatural :: (a -> a) -> Integer -> (Natural -> a) -> a
deriveIntegerFromNatural neg i f
  | i < 0 = neg $ f (naturalFromInteger (-i) :: Natural)
  | otherwise = f (naturalFromInteger i :: Natural)

------------------------------------- Fr --------------------------------------

instance ToConstant Fr where
  type Const Fr = Natural
  toConstant = toConstant . r2h

instance ResidueField Fr where
  type IntegralOf Fr = Integer
  fromIntegral = fromConstant
  toIntegral = toIntegral . r2h

instance Binary Fr where
  put = put . r2h
  get = h2r <$> get

instance Eq Fr where
  (==) a b = r2h a == r2h b

instance ZkFold.Data.Eq.Eq Fr where
  type BooleanOf Fr = Bool
  (==) a b = (==) (r2h a) (r2h b)
  (/=) a b = (/=) (r2h a) (r2h b)

instance P.Enum Fr where
  succ = h2r . P.succ . r2h
  pred = h2r . P.pred . r2h
  toEnum = h2r . P.toEnum
  fromEnum = P.fromEnum . r2h

instance Ord Fr where
  a <= b = (<=) (r2h a) (r2h b)

instance Exponent Fr Natural where
  (^) a n = runRustFun2 r_scalar_exp_natural a (h2r n)

instance Exponent Fr Integer where
  (^) s i = deriveIntegerFromNatural finv i $ (^) s

instance AdditiveSemigroup Fr where
  (+) = runRustFun2 r_scalar_add

instance Zero Fr where
  zero = runRustFun0 r_scalar_zero

instance AdditiveMonoid Fr

instance AdditiveGroup Fr where
  (-) = runRustFun2 r_scalar_sub

  negate = runRustFun1 r_scalar_negate

instance MultiplicativeSemigroup Fr where
  (*) = runRustFun2 r_scalar_mul

instance MultiplicativeMonoid Fr where
  one = runRustFun0 r_scalar_one

instance FromConstant Natural Fr where
  fromConstant = runRustFun1 r_scalar_from_natural . h2r

instance FromConstant Integer Fr where
  fromConstant x = deriveIntegerFromNatural negate x fromConstant

instance Scale Natural Fr where
  scale a = runRustFun2 r_scalar_scale_natural (h2r a)

instance Scale Integer Fr where
  scale a b = deriveIntegerFromNatural negate a $ flip scale b

instance Semiring Fr

instance Ring Fr

instance Field Fr where
  (//) = runRustFun2 r_scalar_div

  finv = runRustFun1 r_scalar_invert

  rootOfUnity n = do
    !a <- rootOfUnity n :: Maybe EC.Fr
    return $ h2r a

------------------------------------ BLS12-381-G1 --------------------------------------

deriving newtype instance Binary Rust_BLS12_381_G1_JacobianPoint

instance Binary Rust_BLS12_381_G1_Point where
  put = put . r2h
  get = h2r <$> get

deriving newtype instance EllipticCurve Rust_BLS12_381_G1_JacobianPoint

instance EllipticCurve Rust_BLS12_381_G1_Point where
  type CurveOf Rust_BLS12_381_G1_Point = "Rust BLS12-381-G1"
  type BaseFieldOf Rust_BLS12_381_G1_Point = EC.Fq

  isOnCurve w = isOnCurve $ r2h w

deriving newtype instance Planar EC.Fq Rust_BLS12_381_G1_JacobianPoint

instance Planar EC.Fq Rust_BLS12_381_G1_Point where
  pointXY a b = h2r $ pointXY a b

instance Binary Rust_BLS12_381_G1_CompressedPoint where
  put = put . r2h
  get = h2r <$> get

deriving newtype instance Compressible Rust_BLS12_381_G1_JacobianPoint

instance Compressible Rust_BLS12_381_G1_Point where
  type Compressed Rust_BLS12_381_G1_Point = Rust_BLS12_381_G1_CompressedPoint
  pointCompressed = error "Not implemented: pointCompressed"
  compress (RPoint w) = RPoint w
  decompress (RPoint w) = RPoint w

instance RustHaskell Rust_BLS12_381_G1_CompressedPoint BLS12_381_G1_CompressedPoint where
  r2h = compress @BLS12_381_G1_Point . r2h . decompress @Rust_BLS12_381_G1_Point

  h2r = compress @Rust_BLS12_381_G1_Point . h2r . decompress @BLS12_381_G1_Point

deriving newtype instance Scale Natural Rust_BLS12_381_G1_JacobianPoint

instance Scale Natural Rust_BLS12_381_G1_Point where
  scale a = runRustFun2 r_g1_scale_natural (h2r a)

deriving newtype instance Scale Integer Rust_BLS12_381_G1_JacobianPoint

instance Scale Integer Rust_BLS12_381_G1_Point where
  scale a b = deriveIntegerFromNatural negate a $ \x -> scale x b

deriving newtype instance AdditiveSemigroup Rust_BLS12_381_G1_JacobianPoint

instance AdditiveSemigroup Rust_BLS12_381_G1_Point where
  (+) = runRustFun2 r_g1_add

deriving newtype instance Zero Rust_BLS12_381_G1_JacobianPoint

deriving newtype instance AdditiveMonoid Rust_BLS12_381_G1_JacobianPoint

instance Zero Rust_BLS12_381_G1_Point where
  zero = runRustFun0 r_g1_zero

instance AdditiveMonoid Rust_BLS12_381_G1_Point

deriving newtype instance AdditiveGroup Rust_BLS12_381_G1_JacobianPoint

instance AdditiveGroup Rust_BLS12_381_G1_Point where
  (-) = runRustFun2 r_g1_sub

  negate = runRustFun1 r_g1_negate

instance Finite Fr where
  type Order Fr = BLS12_381_Scalar

deriving newtype instance Scale Fr Rust_BLS12_381_G1_JacobianPoint

instance Scale Fr Rust_BLS12_381_G1_Point where
  scale = runRustFun2 r_g1_scale

deriving newtype instance CyclicGroup Rust_BLS12_381_G1_JacobianPoint

instance CyclicGroup Rust_BLS12_381_G1_Point where
  type ScalarFieldOf Rust_BLS12_381_G1_Point = Fr

  pointGen = runRustFun0 r_g1_gen

deriving newtype instance Eq Rust_BLS12_381_G1_JacobianPoint

instance Eq Rust_BLS12_381_G1_Point where
  (==) a b = r2h a == r2h b

deriving newtype instance Arbitrary Rust_BLS12_381_G1_JacobianPoint

deriving newtype instance Show Rust_BLS12_381_G1_JacobianPoint

------------------------------------ BLS12-381 G2 ------------------------------------

deriving newtype instance Scale Natural Rust_BLS12_381_G2_JacobianPoint

instance Scale Natural Rust_BLS12_381_G2_Point where
  scale a = runRustFun2 r_g2_scale_natural (h2r a)

deriving newtype instance Scale Integer Rust_BLS12_381_G2_JacobianPoint

instance Scale Integer Rust_BLS12_381_G2_Point where
  scale a b = deriveIntegerFromNatural negate a $ \x -> scale x b

deriving newtype instance AdditiveSemigroup Rust_BLS12_381_G2_JacobianPoint

instance AdditiveSemigroup Rust_BLS12_381_G2_Point where
  (+) = runRustFun2 r_g2_add

deriving newtype instance Zero Rust_BLS12_381_G2_JacobianPoint

deriving newtype instance AdditiveMonoid Rust_BLS12_381_G2_JacobianPoint

instance Zero Rust_BLS12_381_G2_Point where
  zero = runRustFun0 r_g2_zero

instance AdditiveMonoid Rust_BLS12_381_G2_Point

deriving newtype instance AdditiveGroup Rust_BLS12_381_G2_JacobianPoint

instance AdditiveGroup Rust_BLS12_381_G2_Point where
  (-) = runRustFun2 r_g2_sub

  negate = runRustFun1 r_g2_negate

deriving newtype instance Scale Fr Rust_BLS12_381_G2_JacobianPoint

instance Scale Fr Rust_BLS12_381_G2_Point where
  scale = runRustFun2 r_g2_scale

deriving newtype instance CyclicGroup Rust_BLS12_381_G2_JacobianPoint

instance CyclicGroup Rust_BLS12_381_G2_Point where
  type ScalarFieldOf Rust_BLS12_381_G2_Point = Fr

  pointGen = runRustFun0 r_g2_gen

deriving newtype instance Eq Rust_BLS12_381_G2_JacobianPoint

instance Eq Rust_BLS12_381_G2_Point where
  (==) a b = r2h a == r2h b

instance Exponent BLS12_381_GT Fr where
  (^) a p = (^) a (r2h p)

instance Pairing Rust_BLS12_381_G1_Point Rust_BLS12_381_G2_Point BLS12_381_GT where
  pairing a b = pairing (r2h a) (r2h b)

instance Pairing Rust_BLS12_381_G1_JacobianPoint Rust_BLS12_381_G2_JacobianPoint BLS12_381_GT where
  pairing (G1_Jacobian a) (G2_Jacobian b) = pairing a b

-- PolyVec

instance Scale Fr (RustPolyVec Fr size) where
  scale = runRustFun2 r_poly_mul_scalar

instance UnivariateRingPolyVec Fr (RustPolyVec Fr) where
  (.*.) :: forall size. RustPolyVec Fr size -> RustPolyVec Fr size -> RustPolyVec Fr size
  (.*.) = runRustFun2 r_poly_hmul

  (+.) :: forall size. Fr -> RustPolyVec Fr size -> RustPolyVec Fr size
  (+.) = runRustFun2 r_poly_add_scalar

  toPolyVec :: forall size. KnownNat size => V.Vector Fr -> RustPolyVec Fr size
  toPolyVec a = h2r $ toPolyVec (r2h <$> a)

  fromPolyVec :: forall size. KnownNat size => RustPolyVec Fr size -> V.Vector Fr
  fromPolyVec a = h2r <$> fromPolyVec (r2h a)

  evalPolyVec :: forall size. KnownNat size => RustPolyVec Fr size -> Fr -> Fr
  evalPolyVec pv x = h2r $ evalPolyVec (r2h pv) (r2h x)

instance UnivariateFieldPolyVec Fr (RustPolyVec Fr) where
  (./.) :: RustPolyVec Fr size -> RustPolyVec Fr size -> RustPolyVec Fr size
  (./.) = runRustFun2 r_poly_hdiv

  polyVecZero :: forall size. KnownNat size => Natural -> RustPolyVec Fr size
  polyVecZero n = h2r $ polyVecZero n

  polyVecLagrange :: forall size. KnownNat size => Natural -> Natural -> Fr -> RustPolyVec Fr size
  polyVecLagrange n i omega = h2r $ polyVecLagrange n i (r2h omega)

  polyVecInLagrangeBasis :: forall n size. (KnownNat n, KnownNat size) => Fr -> RustPolyVec Fr n -> RustPolyVec Fr size
  polyVecInLagrangeBasis omega pv = h2r $ polyVecInLagrangeBasis (r2h omega) (r2h pv)

  polyVecGrandProduct
    :: forall size
     . KnownNat size
    => RustPolyVec Fr size
    -> RustPolyVec Fr size
    -> RustPolyVec Fr size
    -> Fr
    -> Fr
    -> RustPolyVec Fr size
  polyVecGrandProduct a b sigma beta gamma = h2r $ polyVecGrandProduct (r2h a) (r2h b) (r2h sigma) (r2h beta) (r2h gamma)

  polyVecDiv :: RustPolyVec Fr size -> RustPolyVec Fr size -> RustPolyVec Fr size
  polyVecDiv = runRustFun2 r_poly_div

  divShiftedMono :: RustPolyVec Fr size -> Natural -> Fr -> RustPolyVec Fr size
  divShiftedMono a b = runRustFun3 r_poly_div_shifted_mono a (h2r b)

  castPolyVec :: RustPolyVec Fr size -> RustPolyVec Fr size'
  castPolyVec = RPolyVec . rawPolyVec

instance Scale Natural (RustPolyVec Fr size) where
  scale c = runRustFun2 r_poly_scale_natural (h2r c)

instance KnownNat size => Scale Integer (RustPolyVec Fr size) where
  scale c pv = deriveIntegerFromNatural negate c $ \x -> scale x pv

instance FromConstant Natural (RustPolyVec Fr size) where
  fromConstant = runRustFun1 r_poly_from_natural . h2r

instance KnownNat size => FromConstant Integer (RustPolyVec Fr size) where
  fromConstant n = deriveIntegerFromNatural negate n fromConstant

instance KnownNat size => AdditiveSemigroup (RustPolyVec Fr size) where
  (+) = runRustFun2 r_poly_add

instance Zero (RustPolyVec Fr size) where
  zero = runRustFun0 r_poly_zero

instance KnownNat size => AdditiveMonoid (RustPolyVec Fr size)

instance KnownNat size => AdditiveGroup (RustPolyVec Fr size) where
  (-) = runRustFun2 r_poly_sub

  negate = runRustFun1 r_poly_negate

instance Exponent (RustPolyVec Fr size) Natural where
  (^) pv = runRustFun2 r_poly_exp pv . h2r

instance {-# OVERLAPPING #-} KnownNat size => Scale (RustPolyVec Fr size) (RustPolyVec Fr size)

-- TODO (Issue #18): check for overflow
instance KnownNat size => MultiplicativeSemigroup (RustPolyVec Fr size) where
  (*) = runRustFun2 r_poly_mul

instance KnownNat size => MultiplicativeMonoid (RustPolyVec Fr size) where
  one = runRustFun0 r_poly_one

instance KnownNat size => Semiring (RustPolyVec Fr size)

instance KnownNat size => Ring (RustPolyVec Fr size)

-- TODO: avoid unnecessary casting from Vector to Ptr
instance
  Bilinear
    (RustVector Rust_BLS12_381_G1_Point)
    (RustPolyVec Fr size)
    Rust_BLS12_381_G1_Point
  where
  bilinear = runRustFun2 r_msm

instance
  Bilinear
    (V.Vector Rust_BLS12_381_G1_JacobianPoint)
    (RustPolyVec Fr size)
    Rust_BLS12_381_G1_JacobianPoint
  where
  bilinear points scalars = G1_Jacobian $ bilinear (h2r $ (\(G1_Jacobian p) -> r2h p) <$> points) scalars

-- Arbitrary
instance Arbitrary Fr where
  arbitrary = h2r <$> arbitrary

instance Arbitrary Rust_BLS12_381_G1_Point where
  arbitrary = h2r <$> arbitrary

instance KnownNat size => Arbitrary (RustPolyVec Fr size) where
  arbitrary = h2r <$> arbitrary

deriving newtype instance Arbitrary Rust_BLS12_381_G2_JacobianPoint

instance Arbitrary Rust_BLS12_381_G2_Point where
  arbitrary = h2r <$> arbitrary

-- Show

instance Show Fr where
  show = show . r2h

instance Show Rust_BLS12_381_G1_Point where
  show = show . r2h

instance forall size. KnownNat size => Show (RustPolyVec Fr size) where
  show = show . r2h

deriving newtype instance Show Rust_BLS12_381_G2_JacobianPoint

instance Show Rust_BLS12_381_G2_Point where
  show = show . r2h
