{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.FFI.Rust.RustBLS where

import Control.DeepSeq (force)
import Control.Monad
import qualified Data.Bool
import qualified Data.Vector as V
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Base
import GHC.IO (unsafePerformIO)
import GHC.Natural (naturalFromInteger, naturalToInteger)
import Test.QuickCheck hiding (scale)
import ZkFold.Algebra.Class (sum)
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
import ZkFold.FFI.Rust.Poly
import ZkFold.FFI.Rust.Runner (runRustFun0, runRustFun1, runRustFun2, runRustFun3)
import ZkFold.FFI.Rust.RustFunctions
import ZkFold.FFI.Rust.Types
import ZkFold.Symbolic.MonadCircuit
import Prelude hiding (fromIntegral, negate, sum, (*), (+), (-), (^))
import qualified Prelude as P

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
  (==) a b = (r2h a) == (r2h b)

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
  -- (^) a n = h2r $ (^) (r2h a) n

-- (^) a n = RScalar $ runRustBinary r_scalar_exp_natural (rawScalar a) (rawNatural $ h2r n)
  (^) a n = runRustFun2 r_scalar_exp_natural a (h2r n)

-- (^) a b = h2r $ (^) (r2h a) b

instance Exponent Fr Integer where
  -- (^) a b = h2r $ (^) (r2h a) b

  (^) s i = deriveIntegerFromNatural finv i $ (^) s

instance AdditiveSemigroup Fr where
  -- (+) a b = h2r $ (+) (r2h a) (r2h b)

-- (+) a b = RScalar $ runRustBinary r_scalar_add (rawScalar a) (rawScalar b)
  (+) = runRustFun2 r_scalar_add

instance AdditiveMonoid Fr where
  -- zero = h2r zero

-- zero = RScalar $ runRustConst r_scalar_zero
  zero = runRustFun0 r_scalar_zero

instance AdditiveGroup Fr where
  -- (-) a b = h2r $ (-) (r2h a) (r2h b)

  -- (-) a b = RScalar $ runRustBinary r_scalar_sub (rawScalar a) (rawScalar b)
  (-) = runRustFun2 r_scalar_sub

  -- negate a = h2r $ negate (r2h a)

-- negate a = RScalar $ runRustUnary r_scalar_negate (rawScalar a)
  negate = runRustFun1 r_scalar_negate

instance MultiplicativeSemigroup Fr where
  -- (*) a b = h2r $ (*) (r2h a) (r2h b)

-- (*) a b = RScalar $ runRustBinary r_scalar_mul (rawScalar a) (rawScalar b)
  (*) = runRustFun2 r_scalar_mul

instance MultiplicativeMonoid Fr where
  -- one = h2r one

-- one = RScalar $ runRustConst r_scalar_one
  one = runRustFun0 r_scalar_one

instance FromConstant Natural Fr where
  -- fromConstant = h2r . fromConstant

-- fromConstant n = RScalar $ runRustUnary r_scalar_from_natural (rawNatural $ h2r n)
  fromConstant = runRustFun1 r_scalar_from_natural . h2r

instance FromConstant Integer Fr where
  -- fromConstant = h2r . fromConstant
  fromConstant x = deriveIntegerFromNatural negate x fromConstant

instance Scale Natural Fr where
  -- scale a b = h2r $ scale a (r2h b)

-- scale a b = RScalar $ runRustBinary r_scalar_scale_natural (rawNatural $ h2r a) (rawScalar b)
  scale a = runRustFun2 r_scalar_scale_natural (h2r a)

instance Scale Integer Fr where
  -- scale a b = h2r $ scale a (r2h b)
  scale a b = deriveIntegerFromNatural negate a $ flip scale b

instance Semiring Fr

instance Ring Fr

instance Field Fr where
  -- (//) a b = h2r $ (//) (r2h a) (r2h b)

  -- (//) a b = RScalar $ runRustBinary r_scalar_div (rawScalar a) (rawScalar b)
  (//) = runRustFun2 r_scalar_div

  -- finv x = RScalar $ runRustUnary r_scalar_invert (rawScalar x)
  finv = runRustFun1 r_scalar_invert

  -- finv x = h2r $ finv $ r2h x
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
  -- scale a b = h2r $ scale a (r2h b)

-- scale a b = RPoint $ runRustBinary r_g1_scale_natural (rawNatural $ h2r a) (rawPoint b)
  scale a = runRustFun2 r_g1_scale_natural (h2r a)

deriving newtype instance Scale Integer Rust_BLS12_381_G1_JacobianPoint

instance Scale Integer Rust_BLS12_381_G1_Point where
  -- scale a b = h2r $ scale a (r2h b)
  scale a b = deriveIntegerFromNatural negate a $ \x -> scale x b

deriving newtype instance AdditiveSemigroup Rust_BLS12_381_G1_JacobianPoint

instance AdditiveSemigroup Rust_BLS12_381_G1_Point where
  -- a + b = h2r $ (+) (r2h a) (r2h b)

-- (+) a b = RPoint $ runRustBinary r_g1_add (rawPoint a) (rawPoint b)
  (+) = runRustFun2 r_g1_add

deriving newtype instance AdditiveMonoid Rust_BLS12_381_G1_JacobianPoint

instance AdditiveMonoid Rust_BLS12_381_G1_Point where
  -- zero = h2r zero

-- zero = RPoint $ runRustConst r_g1_zero
  zero = runRustFun0 r_g1_zero

deriving newtype instance AdditiveGroup Rust_BLS12_381_G1_JacobianPoint

instance AdditiveGroup Rust_BLS12_381_G1_Point where
  -- (-) a b = h2r $ (-) (r2h a) (r2h b)

  -- (-) a b = RPoint $ runRustBinary r_g1_sub (rawPoint a) (rawPoint b)
  (-) = runRustFun2 r_g1_sub

  -- negate x = h2r $ negate (r2h x)

-- negate x = RPoint $ runRustUnary r_g1_negate (rawPoint x)
  negate = runRustFun1 r_g1_negate

instance Finite Fr where
  type Order Fr = BLS12_381_Scalar

deriving newtype instance Scale Fr Rust_BLS12_381_G1_JacobianPoint

instance Scale Fr Rust_BLS12_381_G1_Point where
  -- scale scalar point = h2r $ scale (r2h scalar) (r2h point)

-- scale scalar point = RPoint $ runRustBinary r_g1_scale (rawScalar scalar) (rawPoint point)
  scale = runRustFun2 r_g1_scale

deriving newtype instance CyclicGroup Rust_BLS12_381_G1_JacobianPoint

instance CyclicGroup Rust_BLS12_381_G1_Point where
  type ScalarFieldOf Rust_BLS12_381_G1_Point = Fr

  -- pointGen = h2r pointGen

-- pointGen = RPoint $ runRustConst r_g1_gen
  pointGen = runRustFun0 r_g1_gen

deriving newtype instance Eq Rust_BLS12_381_G1_JacobianPoint

instance Eq Rust_BLS12_381_G1_Point where
  (==) a b = r2h a == r2h b

deriving newtype instance Arbitrary Rust_BLS12_381_G1_JacobianPoint

deriving newtype instance Show Rust_BLS12_381_G1_JacobianPoint

------------------------------------ BLS12-381 G2 ------------------------------------

deriving newtype instance Scale Natural Rust_BLS12_381_G2_JacobianPoint

instance Scale Natural Rust_BLS12_381_G2_Point where
  -- scale a b = h2r $ scale a (r2h b)

-- scale a b = RPoint $ runRustBinary r_g2_scale_natural (rawNatural $ h2r a) (rawPoint b)
  scale a = runRustFun2 r_g2_scale_natural (h2r a)

deriving newtype instance Scale Integer Rust_BLS12_381_G2_JacobianPoint

instance Scale Integer Rust_BLS12_381_G2_Point where
  -- scale a b = h2r $ scale a (r2h b)

  scale a b = deriveIntegerFromNatural negate a $ \x -> scale x b

deriving newtype instance AdditiveSemigroup Rust_BLS12_381_G2_JacobianPoint

instance AdditiveSemigroup Rust_BLS12_381_G2_Point where
  -- (+) a b = h2r $ (+) (r2h a) (r2h b)

-- (+) a b = RPoint $ runRustBinary r_g2_add (rawPoint a) (rawPoint b)
  (+) = runRustFun2 r_g2_add

deriving newtype instance AdditiveMonoid Rust_BLS12_381_G2_JacobianPoint

instance AdditiveMonoid Rust_BLS12_381_G2_Point where
  -- zero = h2r zero

-- zero = RPoint $ runRustConst r_g2_zero
  zero = runRustFun0 r_g2_zero

deriving newtype instance AdditiveGroup Rust_BLS12_381_G2_JacobianPoint

instance AdditiveGroup Rust_BLS12_381_G2_Point where
  -- (-) a b = h2r $ (-) (r2h a) (r2h b)

  -- (-) a b = RPoint $ runRustBinary r_g2_sub (rawPoint a) (rawPoint b)
  (-) = runRustFun2 r_g2_sub

  -- negate x = h2r $ negate (r2h x)

-- negate x = RPoint $ runRustUnary r_g2_negate (rawPoint x)
  negate = runRustFun1 r_g2_negate

deriving newtype instance Scale Fr Rust_BLS12_381_G2_JacobianPoint

instance Scale Fr Rust_BLS12_381_G2_Point where
  -- scale s p = h2r $ scale (r2h s) (r2h p)

-- scale s p = RPoint $ runRustBinary r_g2_scale (rawScalar s) (rawPoint p)
  scale = runRustFun2 r_g2_scale

deriving newtype instance CyclicGroup Rust_BLS12_381_G2_JacobianPoint

instance CyclicGroup Rust_BLS12_381_G2_Point where
  type ScalarFieldOf Rust_BLS12_381_G2_Point = Fr

  -- pointGen = RPoint $ runRustConst r_g2_gen
  pointGen = runRustFun0 r_g2_gen

  -- pointGen = h2r pointGen

deriving newtype instance Eq Rust_BLS12_381_G2_JacobianPoint

instance Eq Rust_BLS12_381_G2_Point where
  (==) a b = r2h a == r2h b

-- GT

-- instance Exponent Fq12 Integer where
--   (^) = intPow

-- instance Exponent Fq12 Natural where
--   (^) a n = RScalar $ runRustBinary r_gt_exp_natural (rawScalar a) (rawNatural $ h2r n)

-- instance MultiplicativeSemigroup Fq12 where
--   (*) a b = RScalar $ runRustBinary r_gt_mul (rawScalar a) (rawScalar b)

-- instance MultiplicativeMonoid Fq12 where
--   one = RScalar $ runRustConst r_gt_one

-- instance MultiplicativeGroup Fq12 where
--   (/) a b = RScalar $ runRustBinary r_gt_div (rawScalar a) (rawScalar b)

--   invert a = RScalar $ runRustUnary r_gt_invert (rawScalar a)

-- instance Exponent Fq12 Fr where
--   (^) a p = RScalar $ runRustBinary r_gt_exp (rawScalar a) (rawScalar p)

instance Exponent BLS12_381_GT Fr where
  (^) a p = (^) a (r2h p)

-- instance Pairing Rust_BLS12_381_G1_Point Rust_BLS12_381_G2_Point Fq12 where
-- pairing a b = RScalar $ runRustBinary r_pairing (rawPoint a) (rawPoint b)
instance Pairing Rust_BLS12_381_G1_Point Rust_BLS12_381_G2_Point BLS12_381_GT where
  pairing a b = pairing (r2h a) (r2h b)

-- instance Pairing Rust_BLS12_381_G1_JacobianPoint Rust_BLS12_381_G2_JacobianPoint Fq12 where
--   pairing (G1_Jacobian a) (G2_Jacobian b) = pairing a b

instance Pairing Rust_BLS12_381_G1_JacobianPoint Rust_BLS12_381_G2_JacobianPoint BLS12_381_GT where
  pairing (G1_Jacobian a) (G2_Jacobian b) = pairing a b

-- PolyVec

instance KnownNat size => Scale Fr (RustPolyVec Fr size) where
  -- scale a pv = RPolyVec $ runRustBinary r_poly_mul_scalar (rawScalar a) (rawPolyVec pv)
  scale = runRustFun2 r_poly_mul_scalar

-- scale a pv = h2r $ scale (r2h a) (r2h pv)

instance UnivariateRingPolyVec Fr (RustPolyVec Fr) where
  (.*.) :: forall size. KnownNat size => RustPolyVec Fr size -> RustPolyVec Fr size -> RustPolyVec Fr size
  -- (.*.) a b = RPolyVec $ runRustBinary r_poly_hmul (rawPolyVec a) (rawPolyVec b)
  (.*.) = runRustFun2 r_poly_hmul

  -- (.*.) a b = h2r $ (.*.) (r2h a) (r2h b)

  (+.) :: forall size. KnownNat size => Fr -> RustPolyVec Fr size -> RustPolyVec Fr size
  -- (+.) a b = RPolyVec $ runRustBinary r_poly_add_scalar (rawScalar a) (rawPolyVec b)
  (+.) = runRustFun2 r_poly_add_scalar

  -- (+.) a b = h2r $ (+.) (r2h a) (r2h b)

  toPolyVec :: forall size. KnownNat size => V.Vector Fr -> RustPolyVec Fr size
  toPolyVec a = h2r $ toPolyVec (r2h <$> a)

  fromPolyVec :: forall size. KnownNat size => RustPolyVec Fr size -> V.Vector Fr
  fromPolyVec a = unsafePerformIO $ do
    let a_h = r2h a
    -- print $ "Rust: "
    -- print $ a
    -- print "Haskell: "
    -- print a_h
    return $ h2r <$> fromPolyVec a_h

  evalPolyVec :: forall size. KnownNat size => RustPolyVec Fr size -> Fr -> Fr
  evalPolyVec pv x = h2r $ evalPolyVec (r2h pv) (r2h x)

instance UnivariateFieldPolyVec Fr (RustPolyVec Fr) where
  (./.) :: forall size. KnownNat size => RustPolyVec Fr size -> RustPolyVec Fr size -> RustPolyVec Fr size
  -- l ./. r = h2r $ ((./.) (r2h l) (r2h r) :: PolyVec EC.Fr size)

  -- (./.) l r = RPolyVec $ runRustBinary r_poly_hdiv (rawPolyVec l) (rawPolyVec r)
  (./.) = runRustFun2 r_poly_hdiv

  polyVecZero :: forall size. KnownNat size => Natural -> RustPolyVec Fr size
  polyVecZero n = h2r $ polyVecZero n

  polyVecLagrange :: forall size. KnownNat size => Natural -> Natural -> Fr -> RustPolyVec Fr size
  polyVecLagrange n i omega = h2r $ polyVecLagrange n i (r2h omega)

  -- polyVecLagrange n i omega = RPolyVec $ unsafePerformIO $ do
  --   withForeignPtr
  --     (rawData $ rawNatural $ h2r n)
  --     $ \ptr1 ->
  --       withForeignPtr
  --         (rawData $ rawNatural $ h2r i)
  --         $ \ptr2 -> do
  --           withForeignPtr (rawData $ rawScalar omega) $ \ptr3 -> do
  --             RData <$> (toForeignPtr =<< r_poly_lagrange ptr1 ptr2 ptr3)

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

  polyVecDiv :: forall size. KnownNat size => RustPolyVec Fr size -> RustPolyVec Fr size -> RustPolyVec Fr size
  -- polyVecDiv l r = RPolyVec $ runRustBinary r_poly_div (rawPolyVec l) (rawPolyVec r)
  polyVecDiv = runRustFun2 r_poly_div

  divShiftedMono :: forall size. KnownNat size => RustPolyVec Fr size -> Natural -> Fr -> RustPolyVec Fr size
  -- divShiftedMono a b c = RPolyVec $ runRustTernary r_poly_div_shifted_mono (rawPolyVec a) (rawNatural $ h2r b) (rawScalar c)
  divShiftedMono a b = runRustFun3 r_poly_div_shifted_mono a (h2r b)

  castPolyVec :: forall size size'. (KnownNat size, KnownNat size') => RustPolyVec Fr size -> RustPolyVec Fr size'
  castPolyVec = RPolyVec . rawPolyVec

instance KnownNat size => Scale Natural (RustPolyVec Fr size) where
  -- scale c pv = h2r $ (scale c (r2h pv) :: PolyVec EC.Fr size)

-- scale c pv = RPolyVec $ runRustBinary r_poly_scale_natural (rawPolyVec $ pv) (rawNatural $ h2r c)
  scale c = runRustFun2 r_poly_scale_natural (h2r c)

instance KnownNat size => Scale Integer (RustPolyVec Fr size) where
  -- scale c pv = h2r $ (scale c (r2h pv) :: PolyVec EC.Fr size)

  scale c pv = deriveIntegerFromNatural negate c $ \x -> scale x pv

instance KnownNat size => FromConstant Natural (RustPolyVec Fr size) where
  -- fromConstant n = RPolyVec $ runRustUnary r_poly_from_natural (rawNatural $ h2r n)
  fromConstant = runRustFun1 r_poly_from_natural . h2r

  -- fromConstant n = h2r $ fromConstant n

instance KnownNat size => FromConstant Integer (RustPolyVec Fr size) where
  -- fromConstant n = h2r $ fromConstant n
  fromConstant n = deriveIntegerFromNatural negate n fromConstant

instance KnownNat size => AdditiveSemigroup (RustPolyVec Fr size) where
  -- (+) l r = RPolyVec $ runRustBinary r_poly_add (rawPolyVec l) (rawPolyVec r)
  (+) = runRustFun2 r_poly_add
  -- (+) a b = h2r $ (+) (r2h a) (r2h b)

instance KnownNat size => AdditiveMonoid (RustPolyVec Fr size) where
  -- zero = h2r $ (zero :: PolyVec EC.Fr size)

-- zero = RPolyVec $ runRustConst r_poly_zero
  zero = runRustFun0 r_poly_zero

instance KnownNat size => AdditiveGroup (RustPolyVec Fr size) where
  -- negate pv = h2r $ negate (r2h pv)

  -- (-) a b = RPolyVec $ runRustBinary r_poly_sub (rawPolyVec a) (rawPolyVec b)
  (-) = runRustFun2 r_poly_sub

  -- (-) a b = h2r $ (-) (r2h a) (r2h b)

-- negate = RPolyVec . runRustUnary r_poly_negate . rawPolyVec
  negate = runRustFun1 r_poly_negate

instance KnownNat size => Exponent (RustPolyVec Fr size) Natural where
  -- pv ^ n = h2r $ (^) (r2h pv) n

-- pv ^ n = RPolyVec $ runRustBinary r_poly_exp (rawPolyVec pv) (rawNatural $ h2r n)
  (^) pv = runRustFun2 r_poly_exp pv . h2r

instance {-# OVERLAPPING #-} KnownNat size => Scale (RustPolyVec Fr size) (RustPolyVec Fr size)

-- TODO (Issue #18): check for overflow
instance KnownNat size => MultiplicativeSemigroup (RustPolyVec Fr size) where
  -- (*) l r = RPolyVec $ runRustBinary r_poly_mul (rawPolyVec l) (rawPolyVec r)
  (*) = runRustFun2 r_poly_mul

-- (*) a b = h2r $ (*) (r2h a) (r2h b)

instance KnownNat size => MultiplicativeMonoid (RustPolyVec Fr size) where
  -- one = RPolyVec $ runRustConst r_poly_one
  one = runRustFun0 r_poly_one

instance KnownNat size => Semiring (RustPolyVec Fr size)

instance KnownNat size => Ring (RustPolyVec Fr size)

-- TODO: avoid unnecessary casting from Vector to Ptr
instance
  KnownNat size
  => Bilinear
      (RustVector Rust_BLS12_381_G1_Point)
      (RustPolyVec Fr size)
      Rust_BLS12_381_G1_Point
  where
  -- bilinear points scalars = RPoint $ runRustBinary r_msm (rawPolyVec scalars) (rawVector points)
  bilinear = runRustFun2 r_msm

instance
  KnownNat size
  => Bilinear
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
