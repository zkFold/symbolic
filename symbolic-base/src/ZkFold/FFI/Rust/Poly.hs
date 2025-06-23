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

import ZkFold.Algebra.Number (KnownNat, Natural, value)
import ZkFold.Algebra.Polynomial.Univariate (PolyVec, UnivariateRingPolyVec (..))
import ZkFold.FFI.Rust.Conversion
import ZkFold.FFI.Rust.Types

newtype RustPolyVec a (size :: Natural) = RustPV {rawPoly :: RustData}
  deriving Generic

instance NFData (RustPolyVec a size) where
  rnf _ = ()

peekArrayV :: Storable a => Int -> Ptr a -> IO (V.Vector a)
{-# INLINEABLE peekArrayV #-}
peekArrayV size ptr = V.generateM size (peekElemOff ptr)

pokeArrayV :: Storable a => Ptr a -> V.Vector a -> IO ()
{-# INLINEABLE pokeArrayV #-}
pokeArrayV ptr = V.imapM_ (pokeElemOff ptr)

instance
  ( KnownNat size
  , RustHaskell r h
  , Storable h
  , UnivariateRingPolyVec h (PolyVec h)
  )
  => RustHaskell (RustPolyVec r size) (PolyVec h size)
  where
  h2r pv = unsafePerformIO $ do
    fptr <- callocForeignPtrBytes ((sizeOf (undefined :: h)) * (fromIntegral $ value @size))
    withForeignPtr fptr $ \ptr -> do
      pokeArrayV (castPtr ptr) (fromPolyVec pv)
      return $ RustPV (RData fptr)

  r2h (RustPV rdata) = unsafePerformIO $
    withForeignPtr (rawData rdata) $ \ptr -> do
      let valueSize = (fromIntegral $ value @size)

      l <- peekArrayV valueSize (castPtr $ ptr :: Ptr h)
      return $ toPolyVec @h @(PolyVec h) l
