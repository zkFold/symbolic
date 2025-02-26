{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

module Examples.Blake2b (exampleBlake2b_224, exampleBlake2b_256) where

import           ZkFold.Base.Algebra.Basic.Number        (KnownNat, type (*))
import           ZkFold.Symbolic.Algorithms.Hash.Blake2b (blake2b_224, blake2b_256)
import           ZkFold.Symbolic.Class                   (Symbolic)
import           ZkFold.Symbolic.Data.ByteString         (ByteString)

exampleBlake2b_224 :: (KnownNat n, Symbolic c) => ByteString (8 * n) c -> ByteString 224 c
exampleBlake2b_224 = blake2b_224

exampleBlake2b_256 :: (KnownNat n, Symbolic c) => ByteString (8 * n) c -> ByteString 256 c
exampleBlake2b_256 = blake2b_256
