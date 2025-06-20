{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.FFI.Rust.Types where

import Control.DeepSeq
import Data.Bool as B
import Foreign
import Foreign.C.Types
import GHC.Generics
import ZkFold.Control.Conditional
import Prelude

callocForeignPtrBytes :: Int -> IO (ForeignPtr a)
callocForeignPtrBytes n = do p <- callocBytes n; newForeignPtr finalizerFree p

newtype Scalar curve s = RScalar {rawScalar :: s}
  deriving (NFData, Generic)

newtype Point curve s = RPoint {rawPoint :: s}
  deriving (NFData, Generic)

type FCString = ForeignPtr CChar

instance NFData FCString where
  rnf _ = ()

newtype RustData = RData {rawData :: FCString}
  deriving (NFData, Generic)

instance Conditional Bool RustData where bool = B.bool

-- Scalar BLS

type Fr = Scalar "Rust BLS12-381-G1 Fr" RustData

type Fq = Scalar "Rust BLS12-381-G1 Fq" RustData

-- Point BLS

type Rust_BLS12_381_G1_Point = Point "Rust BLS12-381-G1" RustData

type Rust_BLS12_381_G2_Point = Point "Rust BLS12-381-G2" RustData

-- JacobianPoint BLS

newtype Rust_BLS12_381_G1_JacobianPoint = G1_Jacobian Rust_BLS12_381_G1_Point

newtype Rust_BLS12_381_G2_JacobianPoint = G2_Jacobian Rust_BLS12_381_G2_Point

-- Compressed point BLS

type Rust_BLS12_381_G1_CompressedPoint = Point "Rust BLS12-381-G1 Compressed" RustData

type Rust_BLS12_381_G2_CompressedPoint = Point "Rust BLS12-381-G2 Compressed" RustData
