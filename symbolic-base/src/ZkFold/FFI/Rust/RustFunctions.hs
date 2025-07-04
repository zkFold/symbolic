{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.FFI.Rust.RustFunctions where

import Foreign
import Foreign.C.String
import GHC.IO (unsafePerformIO)
import ZkFold.FFI.Rust.Types
import Prelude

type RustConst =
  IO CString

runRustConst
  :: RustConst
  -> RustData
runRustConst f = unsafePerformIO $ do
  RData <$> (toForeignPtr =<< f)

type RustUnary =
  CString
  -> IO CString

runRustUnary
  :: RustUnary -> RustData -> RustData
runRustUnary f a = unsafePerformIO $ do
  withForeignPtr (rawData a) $ \ptr -> do
    RData <$> (toForeignPtr =<< f ptr)

type RustBinary =
  CString -- first argument
  -> CString -- second argument
  -> IO CString -- output

runRustBinary
  :: RustBinary
  -> RustData
  -> RustData
  -> RustData
runRustBinary f a b = unsafePerformIO $ do
  withForeignPtr (rawData a) $ \ptr1 -> do
    withForeignPtr (rawData b) $ \ptr2 -> do
      RData <$> (toForeignPtr =<< f ptr1 ptr2)

-------------------- Conversion --------------------

-- Scalar

foreign import ccall unsafe "r_h2r_scalar"
  r_h2r_scalar :: CString -> Int -> IO CString

foreign import ccall unsafe "r_r2h_scalar"
  r_r2h_scalar :: CString -> CString -> IO ()

-- Point G1

foreign import ccall unsafe "r_h2r_g1"
  r_h2r_g1 :: CString -> Int -> IO CString

foreign import ccall unsafe "r_r2h_g1"
  r_r2h_g1 :: CString -> CString -> IO ()

-- Point G2

foreign import ccall unsafe "r_h2r_g2"
  r_h2r_g2 :: CString -> Int -> IO CString

foreign import ccall unsafe "r_r2h_g2"
  r_r2h_g2 :: CString -> CString -> IO ()

-- Scalar Vector

foreign import ccall unsafe "r_h2r_scalar_vec"
  r_h2r_scalar_vec :: CString -> Int -> IO CString

foreign import ccall unsafe "r_r2h_scalar_vec"
  r_r2h_scalar_vec :: CString -> CString -> IO ()

-- Scalar Polynomial

foreign import ccall unsafe "r_h2r_scalar_poly"
  r_h2r_scalar_poly :: CString -> Int -> IO CString

foreign import ccall unsafe "r_r2h_scalar_poly"
  r_r2h_scalar_poly :: CString -> CString -> IO ()

-- Point Vector

foreign import ccall unsafe "r_h2r_point_vec"
  r_h2r_point_vec :: CString -> Int -> IO CString

-- GT

foreign import ccall unsafe "r_h2r_gt"
  r_h2r_gt :: CString -> Int -> IO CString

foreign import ccall unsafe "r_r2h_gt"
  r_r2h_gt :: CString -> CString -> IO ()

-------------------- MSM --------------------

foreign import ccall unsafe "r_msm"
  r_msm :: RustBinary

-------------------- Scalar --------------------

-- Const

foreign import ccall unsafe "r_scalar_zero"
  r_scalar_zero :: RustConst

foreign import ccall unsafe "r_scalar_one"
  r_scalar_one :: RustConst

-- Unary

foreign import ccall unsafe "r_scalar_invert"
  r_scalar_invert :: RustUnary

foreign import ccall unsafe "r_scalar_negate"
  r_scalar_negate :: RustUnary

foreign import ccall unsafe "r_scalar_from_natural"
  r_scalar_from_natural :: RustUnary

-- Binary

foreign import ccall unsafe "r_scalar_add"
  r_scalar_add :: RustBinary

foreign import ccall unsafe "r_scalar_mul"
  r_scalar_mul :: RustBinary

foreign import ccall unsafe "r_scalar_scale_natural"
  r_scalar_scale_natural :: RustBinary

foreign import ccall unsafe "r_scalar_sub"
  r_scalar_sub :: RustBinary

foreign import ccall unsafe "r_scalar_exp_natural"
  r_scalar_exp_natural :: RustBinary

foreign import ccall unsafe "r_scalar_div"
  r_scalar_div :: RustBinary

-------------------- Point G1 --------------------

-- Const

foreign import ccall unsafe "r_g1_zero"
  r_g1_zero :: RustConst

foreign import ccall unsafe "r_g1_gen"
  r_g1_gen :: RustConst

-- Unary

foreign import ccall unsafe "r_g1_negate"
  r_g1_negate :: RustUnary

-- Binary

foreign import ccall unsafe "r_g1_add"
  r_g1_add :: RustBinary

foreign import ccall unsafe "r_g1_scale"
  r_g1_scale :: RustBinary

foreign import ccall unsafe "r_g1_scale_natural"
  r_g1_scale_natural :: RustBinary

foreign import ccall unsafe "r_g1_sub"
  r_g1_sub :: RustBinary

-------------------- Point G2 --------------------

-- Const

foreign import ccall unsafe "r_g2_zero"
  r_g2_zero :: RustConst

foreign import ccall unsafe "r_g2_gen"
  r_g2_gen :: RustConst

-- Unary

foreign import ccall unsafe "r_g2_negate"
  r_g2_negate :: RustUnary

-- Binary

foreign import ccall unsafe "r_g2_add"
  r_g2_add :: RustBinary

foreign import ccall unsafe "r_g2_scale_natural"
  r_g2_scale_natural :: RustBinary

foreign import ccall unsafe "r_g2_sub"
  r_g2_sub :: RustBinary

foreign import ccall unsafe "r_g2_scale"
  r_g2_scale :: RustBinary

-------------------- Scalar Polynomial --------------------

-- Const

foreign import ccall unsafe "r_poly_zero"
  r_poly_zero :: RustConst

foreign import ccall unsafe "r_poly_one"
  r_poly_one :: RustConst

-- Unary

foreign import ccall unsafe "r_poly_negate"
  r_poly_negate :: RustUnary

foreign import ccall unsafe "r_poly_from_natural"
  r_poly_from_natural :: RustUnary

-- Binary

foreign import ccall unsafe "r_poly_add"
  r_poly_add :: RustBinary

foreign import ccall unsafe "r_poly_sub"
  r_poly_sub :: RustBinary

foreign import ccall unsafe "r_poly_mul"
  r_poly_mul :: RustBinary

foreign import ccall unsafe "r_poly_div"
  r_poly_div :: RustBinary

foreign import ccall unsafe "r_poly_hmul"
  r_poly_hmul :: RustBinary

foreign import ccall unsafe "r_poly_hdiv"
  r_poly_hdiv :: RustBinary

foreign import ccall unsafe "r_poly_mul_scalar"
  r_poly_mul_scalar :: RustBinary

foreign import ccall unsafe "r_poly_add_scalar"
  r_poly_add_scalar :: RustBinary

foreign import ccall unsafe "r_poly_exp"
  r_poly_exp :: RustBinary

foreign import ccall unsafe "r_poly_scale_natural"
  r_poly_scale_natural :: RustBinary

-------------------- GT --------------------

-- Const

foreign import ccall unsafe "r_gt_one"
  r_gt_one :: RustConst

-- Binary

foreign import ccall unsafe "r_pairing"
  r_pairing :: RustBinary

foreign import ccall unsafe "r_gt_exp"
  r_gt_exp :: RustBinary

foreign import ccall unsafe "r_gt_exp_natural"
  r_gt_exp_natural :: RustBinary

foreign import ccall unsafe "r_gt_mul"
  r_gt_mul :: RustBinary

foreign import ccall unsafe "r_gt_div"
  r_gt_div :: RustBinary

foreign import ccall unsafe "r_gt_invert"
  r_gt_invert :: RustUnary

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
