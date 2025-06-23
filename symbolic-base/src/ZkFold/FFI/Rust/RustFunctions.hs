{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.FFI.Rust.RustFunctions where

import Foreign
import Foreign.C.String
import GHC.IO (unsafePerformIO)
import ZkFold.FFI.Rust.Types
import Prelude

runRustBinary
  :: RustBinary
  -> RustData
  -> RustData
  -> RustData
runRustBinary f a b = unsafePerformIO $ do
  withForeignPtr (rawData a) $ \ptr1 -> do
    withForeignPtr (rawData b) $ \ptr2 -> do
      RData <$> (toForeignPtr =<< f ptr1 ptr2)

type RustBinary =
  CString -- first argument
  -> CString -- second argument
  -> IO CString -- output

-- Conversion

foreign import ccall unsafe "r_h2r_scalar"
  r_h2r_scalar :: CString -> Int -> IO CString

foreign import ccall unsafe "r_r2h_scalar"
  r_r2h_scalar :: CString -> CString -> IO ()

foreign import ccall unsafe "r_h2r_point"
  r_h2r_point :: CString -> Int -> IO CString

foreign import ccall unsafe "r_r2h_point"
  r_r2h_point :: CString -> CString -> IO ()

foreign import ccall unsafe "r_h2r_scalar_vec"
  r_h2r_scalar_vec :: CString -> Int -> IO CString

foreign import ccall unsafe "r_r2h_scalar_vec"
  r_r2h_scalar_vec :: CString -> CString -> IO ()

-- Scale

foreign import ccall unsafe "r_scale"
  r_scale :: RustBinary

-- Scalar

foreign import ccall unsafe "r_scalar_add"
  r_scalar_add :: RustBinary

foreign import ccall unsafe "r_scalar_mul"
  r_scalar_mul :: RustBinary

-- Point

foreign import ccall unsafe "r_point_add"
  r_point_add :: RustBinary

-- Scalar Polynomial

foreign import ccall unsafe "r_poly_add"
  r_poly_add :: RustBinary

foreign import ccall unsafe "r_poly_mul"
  r_poly_mul :: RustBinary

-- foreign import ccall unsafe "rust_wrapper_scalar_mul"
--   rsScalarMul :: RustFunctionBinary

foreign import ccall unsafe "rust_wrapper_plonkup_prove"
  rsPlonkupProve
    :: Int
    -> CString
    -> Int
    -> CString
    -> Int
    -> CString
    -> Int
    -> CString
    -> Int
    -> IO CString
