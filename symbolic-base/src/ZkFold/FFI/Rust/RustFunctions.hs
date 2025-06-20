{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.FFI.Rust.RustFunctions where

import Foreign
import Foreign.C.String
import ZkFold.FFI.Rust.Types
import Prelude

runRustFunctionBinary
  :: RustFunctionBinary
  -> (RustData, Int)
  -> (RustData, Int)
  -> (ForeignPtr a, Int)
  -> IO ()
runRustFunctionBinary f (a, aSize) (b, bSize) (out, outSize) = do
  withForeignPtr (rawData a) $ \ptr1 -> do
    withForeignPtr (rawData b) $ \ptr2 -> do
      withForeignPtr out $ \outPtr -> do
        f
          (castPtr ptr1)
          aSize
          (castPtr ptr2)
          bSize
          outSize
          (castPtr outPtr)

type RustFunctionBinary =
  CString
  -> Int -- first argument
  -> CString
  -> Int -- second argument
  -> Int
  -> CString -- output
  -> IO ()

foreign import ccall unsafe "rust_wrapper_scalar_add"
  rsScalarAdd :: RustFunctionBinary

foreign import ccall unsafe "rust_wrapper_poly_add"
  rsAdd :: RustFunctionBinary

foreign import ccall unsafe "rust_wrapper_mul_fft"
  rsMulFFT :: RustFunctionBinary

foreign import ccall unsafe "rust_wrapper_div_fft"
  rsDivFFT :: RustFunctionBinary

foreign import ccall unsafe "rust_wrapper_msm"
  rsMSM :: RustFunctionBinary

foreign import ccall unsafe "rust_wrapper_scale"
  rsScale :: RustFunctionBinary

foreign import ccall unsafe "rust_wrapper_sum"
  rsSum :: RustFunctionBinary

foreign import ccall unsafe "rust_wrapper_mul"
  rsMul :: RustFunctionBinary

foreign import ccall unsafe "rust_wrapper_hmul"
  rsHMul :: RustFunctionBinary

foreign import ccall unsafe "rust_wrapper_scalar_mul"
  rsScalarMul :: RustFunctionBinary
