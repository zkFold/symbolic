{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.FFI.Rust.RustFunctions where

import Foreign
import Foreign.C.String
import Prelude

type RustFun0 =
  IO CString

type RustFun1 =
  CString
  -> IO CString

type RustFun2 =
  CString
  -> CString
  -> IO CString

type RustFun3 =
  CString
  -> CString
  -> CString
  -> IO CString

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

-------------------- MSM --------------------

foreign import ccall unsafe "r_msm"
  r_msm :: RustFun2

-------------------- Scalar --------------------

-- Const

foreign import ccall unsafe "r_scalar_zero"
  r_scalar_zero :: RustFun0

foreign import ccall unsafe "r_scalar_one"
  r_scalar_one :: RustFun0

-- Unary

foreign import ccall unsafe "r_scalar_invert"
  r_scalar_invert :: RustFun1

foreign import ccall unsafe "r_scalar_negate"
  r_scalar_negate :: RustFun1

foreign import ccall unsafe "r_scalar_from_natural"
  r_scalar_from_natural :: RustFun1

-- Binary

foreign import ccall unsafe "r_scalar_add"
  r_scalar_add :: RustFun2

foreign import ccall unsafe "r_scalar_mul"
  r_scalar_mul :: RustFun2

foreign import ccall unsafe "r_scalar_scale_natural"
  r_scalar_scale_natural :: RustFun2

foreign import ccall unsafe "r_scalar_sub"
  r_scalar_sub :: RustFun2

foreign import ccall unsafe "r_scalar_exp_natural"
  r_scalar_exp_natural :: RustFun2

foreign import ccall unsafe "r_scalar_div"
  r_scalar_div :: RustFun2

-------------------- Point G1 --------------------

-- Const

foreign import ccall unsafe "r_g1_zero"
  r_g1_zero :: RustFun0

foreign import ccall unsafe "r_g1_gen"
  r_g1_gen :: RustFun0

-- Unary

foreign import ccall unsafe "r_g1_negate"
  r_g1_negate :: RustFun1

-- Binary

foreign import ccall unsafe "r_g1_add"
  r_g1_add :: RustFun2

foreign import ccall unsafe "r_g1_scale"
  r_g1_scale :: RustFun2

foreign import ccall unsafe "r_g1_scale_natural"
  r_g1_scale_natural :: RustFun2

foreign import ccall unsafe "r_g1_sub"
  r_g1_sub :: RustFun2

-------------------- Point G2 --------------------

-- Const

foreign import ccall unsafe "r_g2_zero"
  r_g2_zero :: RustFun0

foreign import ccall unsafe "r_g2_gen"
  r_g2_gen :: RustFun0

-- Unary

foreign import ccall unsafe "r_g2_negate"
  r_g2_negate :: RustFun1

-- Binary

foreign import ccall unsafe "r_g2_add"
  r_g2_add :: RustFun2

foreign import ccall unsafe "r_g2_scale_natural"
  r_g2_scale_natural :: RustFun2

foreign import ccall unsafe "r_g2_sub"
  r_g2_sub :: RustFun2

foreign import ccall unsafe "r_g2_scale"
  r_g2_scale :: RustFun2

-------------------- Scalar Polynomial --------------------

-- Const

foreign import ccall unsafe "r_poly_zero"
  r_poly_zero :: RustFun0

foreign import ccall unsafe "r_poly_one"
  r_poly_one :: RustFun0

-- Unary

foreign import ccall unsafe "r_poly_negate"
  r_poly_negate :: RustFun1

foreign import ccall unsafe "r_poly_from_natural"
  r_poly_from_natural :: RustFun1

-- Binary

foreign import ccall unsafe "r_poly_add"
  r_poly_add :: RustFun2

foreign import ccall unsafe "r_poly_sub"
  r_poly_sub :: RustFun2

foreign import ccall unsafe "r_poly_mul"
  r_poly_mul :: RustFun2

foreign import ccall unsafe "r_poly_div"
  r_poly_div :: RustFun2

foreign import ccall unsafe "r_poly_hmul"
  r_poly_hmul :: RustFun2

foreign import ccall unsafe "r_poly_hdiv"
  r_poly_hdiv :: RustFun2

foreign import ccall unsafe "r_poly_mul_scalar"
  r_poly_mul_scalar :: RustFun2

foreign import ccall unsafe "r_poly_add_scalar"
  r_poly_add_scalar :: RustFun2

foreign import ccall unsafe "r_poly_exp"
  r_poly_exp :: RustFun2

foreign import ccall unsafe "r_poly_scale_natural"
  r_poly_scale_natural :: RustFun2

-- Ternary

foreign import ccall unsafe "r_poly_div_shifted_mono"
  r_poly_div_shifted_mono :: RustFun3

-- Plonkup prove

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
