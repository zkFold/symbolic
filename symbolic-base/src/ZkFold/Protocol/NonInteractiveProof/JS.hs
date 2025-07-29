{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Protocol.NonInteractiveProof.Internal where

import Control.DeepSeq (NFData, force)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import qualified Data.Vector as V
import Data.Word (Word8)
import GHC.JS.Prim
import Numeric.Natural (Natural)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (Num ((*)), sum)

import ZkFold.Algebra.Class (Bilinear (..), Scale (..), sum)
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Algebra.Polynomial.Univariate (PolyVec, UnivariateRingPolyVec (..), fromPolyVec)
import ZkFold.Data.Binary
import ZkFold.Protocol.NonInteractiveProof.Class

foreign import javascript unsafe "blake2b"
  blake2b :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

instance Binary a => FromTranscript ByteString a where
  fromTranscript = fromJust . fromByteString . (hsBlake2b 28 mempty)
   where
    hsBlake2b :: Int -> ByteString -> ByteString -> ByteString
    hsBlake2b a b c =
      fromString $
        fromJSString (unsafePerformIO $ blake2b (toJSString $ C.unpack c) (toJSString $ C.unpack b) (toJSInt a) jsNull jsNull)
