{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE Unsafe              #-}

module ZkFold.Base.Protocol.NonInteractiveProof.Internal where

#if defined(javascript_HOST_ARCH)
import qualified Data.ByteString.Char8                      as C
import           Data.String                                (IsString(fromString))
import           GHC.JS.Prim
import           System.IO.Unsafe                           (unsafePerformIO)
#else
import           Crypto.Hash.BLAKE2.BLAKE2b                 (hash)
#endif

import           Data.ByteString                            (ByteString)
import           Data.Maybe                                 (fromJust)
import qualified Data.Vector                                as V
import           Data.Word                                  (Word8)
import           Numeric.Natural                            (Natural)
import           Prelude                                    hiding (Num ((*)), sum)

import           ZkFold.Base.Algebra.Basic.Class            (Field, MultiplicativeSemigroup ((*)), Scale (..), sum)
import           ZkFold.Base.Algebra.EllipticCurve.Class    (CyclicGroup (..))
import           ZkFold.Base.Algebra.Polynomials.Univariate (Poly, PolyVec, fromPolyVec)
import           ZkFold.Base.Data.ByteString

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

class NonInteractiveProof a core where
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

class (CyclicGroup g) => CoreFunction g core where
    msm :: (f ~ ScalarFieldOf g) => V.Vector g -> PolyVec f size -> g

    polyMul :: (f ~ ScalarFieldOf g, Field f, Eq f) => Poly f -> Poly f -> Poly f

data HaskellCore

instance (CyclicGroup g, f ~ ScalarFieldOf g) => CoreFunction g HaskellCore where
    msm gs f = sum $ V.zipWith scale (fromPolyVec f) gs
    polyMul = (*)
