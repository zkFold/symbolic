{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Protocol.NonInteractiveProof.Haskell where

import Crypto.Hash.BLAKE2.BLAKE2b (hash)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Prelude hiding (Num ((*)), sum)

import ZkFold.Data.Binary
import ZkFold.Protocol.NonInteractiveProof.Class

instance Binary a => FromTranscript ByteString a where
  fromTranscript = fromJust . fromByteString . hash 28 mempty
