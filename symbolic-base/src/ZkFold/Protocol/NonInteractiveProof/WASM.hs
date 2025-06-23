{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Protocol.NonInteractiveProof.WASM where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import Data.Word (Word8)
import Foreign.Marshal.Array (newArray)
import Foreign.Ptr (Ptr)
import GHC.Wasm.Prim
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)
import Prelude hiding (Num ((*)), sum)

import ZkFold.Data.ByteString
import ZkFold.Protocol.NonInteractiveProof.Class

foreign import javascript unsafe "blake2b($1, $2, $3, $4, $5)"
  blake2b :: JSVal -> JSString -> Int32 -> Int32 -> Int32 -> IO JSString

foreign import javascript unsafe "new Uint8Array(__exports.memory.buffer, $1, $2)"
  js_toUint8Array :: Ptr Word8 -> Int -> IO JSVal

foreign import javascript unsafe "console.log($1)"
  js_print :: JSString -> IO ()

-- | traces do not work in wasm. For pure debug logs in wasm, this function must me used.
mentallyBrokenJsTrace :: Show a => String -> a -> a
mentallyBrokenJsTrace string expr = unsafePerformIO $ do
  js_print $ toJSString (string <> show expr)
  return expr

bsToUint8Array :: ByteString -> JSVal
bsToUint8Array bs = unsafeDupablePerformIO $ do
  let l = BS.length bs
      w = BS.unpack bs
  arr <- newArray w
  js_toUint8Array arr l

instance Binary a => FromTranscript ByteString a where
  fromTranscript = fromJust . fromByteString . hsBlake2b
   where
    hsBlake2b :: ByteString -> ByteString
    hsBlake2b c =
      fromString $ fromJSString (unsafePerformIO $ blake2b (bsToUint8Array c) (toJSString "") 28 0 0)
