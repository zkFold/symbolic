{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ZkFold.FFI.Rust.Poly where

import Control.DeepSeq (NFData (..))
import qualified Data.Vector as V
import Foreign
import GHC.Base
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import Prelude hiding (drop, length, product, replicate, sum, take, (/), (^))

import           ZkFold.Algebra.Number                (KnownNat, Natural, value)
import           ZkFold.Algebra.Polynomial.Univariate (PolyVec, UnivariateRingPolyVec (..))
import           ZkFold.FFI.Rust.Conversion
import           ZkFold.FFI.Rust.Types
import           Foreign.C.Types
import           Foreign.C.String
import           ZkFold.FFI.Rust.RustFunctions
import qualified ZkFold.Algebra.EllipticCurve.BLS12_381 as EC
newtype RustPolyVec a (size :: Natural) = RPolyVec { rawPolyVec :: RustData }
    deriving (Generic)

instance NFData (RustPolyVec a size) where
  rnf _ = ()

peekArrayV :: Storable a => Int -> Ptr a -> IO (V.Vector a)
{-# INLINEABLE peekArrayV #-}
peekArrayV size ptr = V.generateM size (peekElemOff ptr)

pokeArrayV :: Storable a => Ptr a -> V.Vector a -> IO ()
{-# INLINEABLE pokeArrayV #-}
pokeArrayV ptr = V.imapM_ (pokeElemOff ptr)

o2nScalarVec :: forall size . (KnownNat size) => (RustPolyVec Fr size) -> (RustPolyVec Fr size)
o2nScalarVec old = unsafePerformIO $ do
    withForeignPtr (rawData $ rawPolyVec old) $ \ptr -> do
        ptrNew <- r_h2r_scalar_vec ptr ((sizeOf (undefined :: Fr)) * valueSize)
        RPolyVec . RData <$> newForeignPtr finalizerFree ptrNew
    where
        valueSize = fromIntegral $ value @size

n2oScalarVec :: forall size . (KnownNat size) => (RustPolyVec Fr size) -> (RustPolyVec Fr size)
n2oScalarVec new = unsafePerformIO $ do
    out <- callocForeignPtrBytes @CChar ((sizeOf (undefined :: Fr)) * valueSize)
    withForeignPtr (rawData $ rawPolyVec new) $ \ptr -> do
        withForeignPtr out $ \optr -> do
            r_r2h_scalar_vec ptr optr
    return $ RPolyVec $ RData  out
    where
        valueSize = fromIntegral $ value @size

instance
    ( KnownNat size
    ) => RustHaskell (RustPolyVec Fr size) (PolyVec EC.Fr size) where

    h2r pv = unsafePerformIO $ do
        fptr <- callocForeignPtrBytes ((sizeOf (undefined :: EC.Fr)) * (fromIntegral $ value @size))
        withForeignPtr fptr $ \ptr -> do
            pokeArrayV (castPtr ptr) (fromPolyVec pv)
            return $ o2nScalarVec $ RPolyVec (RData fptr)

    r2h pv = unsafePerformIO $
        withForeignPtr (rawData $ rawPolyVec $ n2oScalarVec pv) $ \ptr -> do
            let valueSize = (fromIntegral $ value @size)

            l <- peekArrayV valueSize (castPtr $ ptr :: Ptr EC.Fr)
            return $ toPolyVec @EC.Fr @(PolyVec EC.Fr) l
