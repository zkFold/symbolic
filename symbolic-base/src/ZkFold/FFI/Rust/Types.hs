{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.FFI.Rust.Types where

import Control.DeepSeq
import Data.Bool as B
import Foreign
import Foreign.C (CString)
import Foreign.C.Types
import GHC.Generics
import GHC.IO (unsafePerformIO)
import GHC.Natural (Natural)
import ZkFold.Control.Conditional
import Prelude

type FCString = ForeignPtr CChar

type RustFinalizer = FunPtr (CString -> IO ())

newtype RustData = RData {rawData :: FCString}
  deriving Generic

instance NFData RustData where
  rnf _ = ()

class IsRustType r where
  rawType :: r -> RustData

  toRustType :: CString -> r

newtype Scalar curve s = RScalar {rawScalar :: s}
  deriving (Generic, NFData)

newtype Point curve s = RPoint {rawPoint :: s}
  deriving Generic

newtype RustNatural = RNatural {rawNatural :: RustData}

instance IsRustType RustNatural where
  rawType = rawNatural
  toRustType = RNatural . toForeignPtr finalizerFree

newtype RustPolyVec a (size :: Natural) = RPolyVec {rawPolyVec :: RustData}
  deriving Generic

foreign import ccall unsafe "&r_scalar_poly_free"
  r_scalar_poly_free :: RustFinalizer

instance IsRustType (RustPolyVec Fr size) where
  rawType = rawPolyVec
  toRustType = RPolyVec . toForeignPtr r_scalar_poly_free

data RustVector a = RVector {length :: Int, rawVector :: RustData}
  deriving Generic

foreign import ccall unsafe "&r_point_vec_free"
  r_point_vec_free :: RustFinalizer

instance IsRustType (RustVector Rust_BLS12_381_G1_Point) where
  rawType = rawVector
  toRustType = error "do not return point vector"

foreign import ccall unsafe "&r_scalar_vec_free"
  r_scalar_vec_free :: RustFinalizer

instance IsRustType (RustVector Fr) where
  rawType = rawVector
  toRustType = error "do not return scalar vector"

toForeignPtr :: RustFinalizer -> CString -> RustData
toForeignPtr finalizer = RData . unsafePerformIO . newForeignPtr finalizer

---------- BLS types ----------

-- Scalar BLS

type Fr = Scalar "Rust BLS12-381-G1" RustData

foreign import ccall unsafe "&r_scalar_free"
  r_scalar_free :: RustFinalizer

instance IsRustType Fr where
  rawType = rawScalar
  toRustType = RScalar . toForeignPtr r_scalar_free

-- Point G1 BLS

type Rust_BLS12_381_G1_Point = Point "Rust BLS12-381-G1" RustData

foreign import ccall unsafe "&r_g1_free"
  r_g1_free :: RustFinalizer

instance IsRustType Rust_BLS12_381_G1_Point where
  rawType = rawPoint
  toRustType = RPoint . toForeignPtr r_g1_free

-- Point G2 BLS

type Rust_BLS12_381_G2_Point = Point "Rust BLS12-381-G2" RustData

foreign import ccall unsafe "&r_g2_free"
  r_g2_free :: RustFinalizer

instance IsRustType Rust_BLS12_381_G2_Point where
  rawType = rawPoint
  toRustType = RPoint . toForeignPtr r_g2_free

-- JacobianPoint BLS

newtype Rust_BLS12_381_G1_JacobianPoint = G1_Jacobian Rust_BLS12_381_G1_Point

newtype Rust_BLS12_381_G2_JacobianPoint = G2_Jacobian Rust_BLS12_381_G2_Point

-- Compressed point BLS

type Rust_BLS12_381_G1_CompressedPoint = Point "Rust BLS12-381-G1 Compressed" RustData

type Rust_BLS12_381_G2_CompressedPoint = Point "Rust BLS12-381-G2 Compressed" RustData


instance Conditional Bool RustData where bool = B.bool
