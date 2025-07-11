module ZkFold.FFI.Rust.Runner where

import Foreign (newForeignPtr)
import Foreign.ForeignPtr (withForeignPtr)
import GHC.IO (unsafePerformIO)
import ZkFold.FFI.Rust.RustFunctions
import ZkFold.FFI.Rust.Types
import Prelude

runRustFun0
  :: forall r
   . IsRustType r
  => RustFun0
  -> r
runRustFun0 f = unsafePerformIO $ toRustType <$> f

runRustFun1 :: forall a1 r. (IsRustType r, IsRustType a1) => RustFun1 -> a1 -> r
runRustFun1 f a =
  unsafePerformIO $
    toRustType <$> do
      withForeignPtr (rawData $ rawType a) f

runRustFun2 :: forall a1 a2 r. (IsRustType r, IsRustType a1, IsRustType a2) => RustFun2 -> a1 -> a2 -> r
runRustFun2 f a1 a2 =
  unsafePerformIO $
    toRustType <$> do
      withForeignPtr (rawData $ rawType a1) $ \ptr1 ->
        withForeignPtr (rawData $ rawType a2) $ f ptr1

runRustFun3
  :: forall a1 a2 a3 r. (IsRustType r, IsRustType a1, IsRustType a2, IsRustType a3) => RustFun3 -> a1 -> a2 -> a3 -> r
runRustFun3 f a1 a2 a3 =
  unsafePerformIO $
    toRustType <$> do
      withForeignPtr (rawData $ rawType a1) $ \ptr1 ->
        withForeignPtr (rawData $ rawType a2) $ \ptr2 ->
          withForeignPtr (rawData $ rawType a3) $ f ptr1 ptr2
