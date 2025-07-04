{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ZkFold.FFI.Rust.Poly where

import Control.DeepSeq (NFData (..))
import qualified Data.Vector as V
import Foreign hiding (new)
import Foreign.C.String
import Foreign.C.Types
import GHC.Base
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import GHC.Natural (shiftLNatural)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import qualified ZkFold.Algebra.EllipticCurve.BLS12_381 as EC
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import ZkFold.Algebra.Number (
  KnownNat,
  Natural,
  value,
 )
import ZkFold.Algebra.Polynomial.Univariate (
  PolyVec,
  UnivariateRingPolyVec (..),
 )
import ZkFold.FFI.Rust.Conversion
import ZkFold.FFI.Rust.RustFunctions
import ZkFold.FFI.Rust.Types
import Prelude hiding (
  drop,
  length,
  product,
  replicate,
  sum,
  take,
  (/),
  (^),
 )

newtype RustPolyVec a (size :: Natural) = RPolyVec {rawPolyVec :: RustData}
  deriving Generic

data RustVector a = RVector {length :: Int, rawVector :: RustData}
  deriving Generic

instance NFData (RustPolyVec a size) where
  rnf _ = ()

peekArrayV :: Storable a => Int -> Ptr a -> IO (V.Vector a)
{-# INLINEABLE peekArrayV #-}
peekArrayV size ptr = V.generateM size (peekElemOff ptr)

pokeArrayV :: Storable a => Ptr a -> V.Vector a -> IO ()
{-# INLINEABLE pokeArrayV #-}
pokeArrayV ptr = V.imapM_ (pokeElemOff ptr)

-- o2n - call Rust function which convert Haskell serialized view into Rust object and return pointer on it
-- n20 - call Rust function which convert pointer on Rust object into Haskell bytes

o2nScalarVec :: forall size. KnownNat size => (RustPolyVec Fr size) -> (RustPolyVec Fr size)
o2nScalarVec old = unsafePerformIO $ do
  withForeignPtr (rawData $ rawPolyVec old) $ \ptr -> do
    ptrNew <- r_h2r_scalar_vec ptr ((sizeOf (undefined :: Fr)) * valueSize)
    RPolyVec . RData <$> newForeignPtr finalizerFree ptrNew
 where
  valueSize = fromIntegral $ value @size

n2oScalarVec :: forall size. KnownNat size => (RustPolyVec Fr size) -> (RustPolyVec Fr size)
n2oScalarVec new = unsafePerformIO $ do
  out <- callocForeignPtrBytes @CChar ((sizeOf (undefined :: Fr)) * valueSize)
  withForeignPtr (rawData $ rawPolyVec new) $ \ptr -> do
    withForeignPtr out $ \optr -> do
      r_r2h_scalar_vec ptr optr
  return $ RPolyVec $ RData out
 where
  valueSize = fromIntegral $ value @size

o2nScalarPoly :: forall size. KnownNat size => (RustPolyVec Fr size) -> (RustPolyVec Fr size)
o2nScalarPoly old = unsafePerformIO $ do
  withForeignPtr (rawData $ rawPolyVec old) $ \ptr -> do
    ptrNew <- r_h2r_scalar_poly ptr ((sizeOf (undefined :: Fr)) * valueSize)
    RPolyVec . RData <$> newForeignPtr finalizerFree ptrNew
 where
  valueSize = fromIntegral $ value @size

n2oScalarPoly :: forall size. KnownNat size => (RustPolyVec Fr size) -> (RustPolyVec Fr size)
n2oScalarPoly new = unsafePerformIO $ do
  out <- callocForeignPtrBytes @CChar ((sizeOf (undefined :: Fr)) * valueSize)
  withForeignPtr (rawData $ rawPolyVec new) $ \ptr -> do
    withForeignPtr out $ \optr -> do
      r_r2h_scalar_poly ptr optr
  return $ RPolyVec $ RData out
 where
  valueSize = fromIntegral $ value @size

o2nPointVec :: RustVector Rust_BLS12_381_G1_Point -> RustVector Rust_BLS12_381_G1_Point
o2nPointVec old = unsafePerformIO $ do
  withForeignPtr (rawData $ rawVector old) $ \ptr -> do
    ptrNew <- r_h2r_point_vec ptr ((sizeOf (undefined :: Rust_BLS12_381_G1_Point)) * valueSize)
    RVector valueSize . RData <$> newForeignPtr finalizerFree ptrNew
 where
  valueSize = length old

instance
  KnownNat size
  => RustHaskell (RustPolyVec Fr size) (PolyVec EC.Fr size)
  where
  h2r pv = unsafePerformIO $ do
    fptr <- callocForeignPtrBytes ((sizeOf (undefined :: EC.Fr)) * (fromIntegral $ value @size))
    withForeignPtr fptr $ \ptr -> do
      pokeArrayV (castPtr ptr) (fromPolyVec pv)
      return $ o2nScalarPoly $ RPolyVec (RData fptr)

  r2h pv = unsafePerformIO $
    withForeignPtr (rawData $ rawPolyVec $ n2oScalarPoly pv) $ \ptr -> do
      let valueSize = (fromIntegral $ value @size)

      l <- peekArrayV valueSize (castPtr $ ptr :: Ptr EC.Fr)
      return $ toPolyVec @EC.Fr @(PolyVec EC.Fr) l

instance RustHaskell (RustVector Rust_BLS12_381_G1_Point) (V.Vector EC.BLS12_381_G1_Point) where
  h2r vec = unsafePerformIO $ do
    fptr <- callocForeignPtrBytes ((sizeOf (undefined :: EC.BLS12_381_G1_Point)) * (V.length vec))
    withForeignPtr fptr $ \ptr -> do
      pokeArrayV (castPtr ptr) vec
    return $ o2nPointVec $ RVector (V.length vec) (RData fptr)

  r2h = error "We don't want to use r2h for point vector"

naturalSize :: Int
naturalSize = 32

instance RustHaskell RustNatural Natural where
  h2r n
    | n >= (1 `shiftLNatural` (8 * naturalSize)) = error "too big natural"
    | otherwise = unsafePerformIO $ do
        fptr <- callocForeignPtrBytes @CChar naturalSize
        withForeignPtr fptr $ \ptr -> do
          -- !_ <- poke (castPtr ptr `plusPtr` 3) naturalSize
          !_ <- pokeNatural (castPtr ptr) n
          return ()
        return $ RNatural (RData fptr)

  r2h r = unsafePerformIO $ withForeignPtr (rawData $ rawNatural r) $ peekNatural naturalSize . castPtr
