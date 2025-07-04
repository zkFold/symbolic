{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.FFI.Rust.Types where

import Control.DeepSeq
import Data.Bool as B
import Foreign
import Foreign.C.Types
import GHC.Generics
import ZkFold.Control.Conditional
import Prelude

toForeignPtr :: Ptr a -> IO (ForeignPtr a)
toForeignPtr = newForeignPtr finalizerFree

callocForeignPtrBytes :: Int -> IO (ForeignPtr a)
callocForeignPtrBytes n = do p <- callocBytes n; toForeignPtr p

newtype Scalar curve s = RScalar {rawScalar :: s}
  deriving (Generic, NFData)

newtype Point curve s = RPoint {rawPoint :: s}
  deriving (Generic, NFData)

type FCString = ForeignPtr CChar

instance NFData FCString where
  rnf _ = ()

newtype RustData = RData {rawData :: FCString}
  deriving (Generic, NFData)

instance Conditional Bool RustData where bool = B.bool

newtype RustNatural = RNatural {rawNatural :: RustData}

-- Scalar BLS

type Fr = Scalar "Rust BLS12-381-G1" RustData

-- type Fq = Scalar "Rust BLS12-381-G1 Fq" RustData

-- Point BLS

type Rust_BLS12_381_G1_Point = Point "Rust BLS12-381-G1" RustData

type Rust_BLS12_381_G2_Point = Point "Rust BLS12-381-G2" RustData

type Fq12 = Scalar "Rust BLS12-381-G1 Fq12" RustData

-- JacobianPoint BLS

newtype Rust_BLS12_381_G1_JacobianPoint = G1_Jacobian Rust_BLS12_381_G1_Point

newtype Rust_BLS12_381_G2_JacobianPoint = G2_Jacobian Rust_BLS12_381_G2_Point

-- Compressed point BLS

type Rust_BLS12_381_G1_CompressedPoint = Point "Rust BLS12-381-G1 Compressed" RustData

type Rust_BLS12_381_G2_CompressedPoint = Point "Rust BLS12-381-G2 Compressed" RustData
