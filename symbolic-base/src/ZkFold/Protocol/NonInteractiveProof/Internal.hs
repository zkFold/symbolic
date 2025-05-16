{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe               #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Protocol.NonInteractiveProof.Internal where

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import qualified Data.ByteString.Char8                      as C
import           Data.String                                (IsString(fromString))
import           System.IO.Unsafe                           (unsafePerformIO)
#else
import           Crypto.Hash.BLAKE2.BLAKE2b                 (hash)
#endif
#if defined(javascript_HOST_ARCH)
import           GHC.JS.Prim
#elif defined(wasm32_HOST_ARCH)
import           System.IO.Unsafe      (unsafeDupablePerformIO)
import           GHC.Wasm.Prim
import           Data.Int              (Int32)
import qualified Data.ByteString       as BS
import           Foreign.Marshal.Array (newArray)
import           Foreign.Ptr           (Ptr)
#endif

import           Control.DeepSeq                            (NFData, force)
import           Data.ByteString                            (ByteString)
import           Data.Maybe                                 (fromJust)
import qualified Data.Vector                                as V
import           Data.Word                                  (Word8)
import           Numeric.Natural                            (Natural)
import           Prelude                                    hiding (Num ((*)), sum)

import           ZkFold.Algebra.Class                 (Bilinear (..), Scale (..), sum)
import           ZkFold.Algebra.Number                (KnownNat)
import           ZkFold.Algebra.EllipticCurve.Class   (CyclicGroup (..))
import           ZkFold.Algebra.Polynomial.Univariate (PolyVec, UnivariateRingPolyVec (..), fromPolyVec)
import           ZkFold.Data.ByteString

class Monoid ts => ToTranscript ts a where
    toTranscript :: a -> ts

instance Binary a => ToTranscript ByteString a where
    toTranscript = toByteString

transcript :: ToTranscript ts a => ts -> a -> ts
transcript ts a = ts <> toTranscript a

class Monoid ts => FromTranscript ts a where
    fromTranscript :: ts -> a

#if defined(javascript_HOST_ARCH)

foreign import javascript unsafe "blake2b"
    blake2b :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

instance Binary a => FromTranscript ByteString a where
    fromTranscript = fromJust . fromByteString . (hsBlake2b 28 mempty)
        where
            hsBlake2b :: Int -> ByteString -> ByteString -> ByteString
            hsBlake2b a b c =
                fromString $ fromJSString (unsafePerformIO $ blake2b (toJSString $ C.unpack c) (toJSString $ C.unpack b) (toJSInt a) jsNull jsNull)

#elif defined(wasm32_HOST_ARCH)

foreign import javascript unsafe "blake2b($1, $2, $3, $4, $5)"
    blake2b :: JSVal -> JSString -> Int32 -> Int32 -> Int32 -> IO JSString

foreign import javascript unsafe "new Uint8Array(__exports.memory.buffer, $1, $2)"
  js_toUint8Array :: Ptr Word8 -> Int -> IO JSVal

foreign import javascript unsafe "console.log($1)"
  js_print :: JSString -> IO ()

-- | traces do not work in wasm. For pure debug logs in wasm, this function must me used.
--
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
#else

instance Binary a => FromTranscript ByteString a where
    fromTranscript = fromJust . fromByteString . hash 28 mempty

#endif

challenge :: forall ts a . FromTranscript ts a => ts -> a
challenge = fromTranscript

challenges :: (ToTranscript ts Word8, FromTranscript ts a) => ts -> Natural -> ([a], ts)
challenges ts0 n = go ts0 n []
  where
    go ts 0 acc = (acc, ts)
    go ts k acc =
        let c   = challenge ts
            ts' = ts `transcript` (0 :: Word8)
        in go ts' (k - 1) (c : acc)

class NonInteractiveProof a where
    type Transcript a

    type SetupProve a

    type SetupVerify a

    type Witness a

    type Input a

    type Proof a

    setupProve :: a -> SetupProve a

    setupVerify :: a -> SetupVerify a

    prove :: SetupProve a -> Witness a -> (Input a, Proof a)

    verify :: SetupVerify a -> Input a -> Proof a -> Bool


instance
    ( CyclicGroup g
    , KnownNat size
    , NFData g
    , f ~ ScalarFieldOf g
    , UnivariateRingPolyVec f (PolyVec f)
    ) => Bilinear (V.Vector g) (PolyVec f size) g where
    bilinear gs f = sum $ V.zipWith (\a b -> force $ scale a b) (fromPolyVec f) gs
