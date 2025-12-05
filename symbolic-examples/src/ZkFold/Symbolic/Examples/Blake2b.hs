{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module ZkFold.Symbolic.Examples.Blake2b (exampleBlake2b_224, exampleBlake2b_256) where

import ZkFold.Algebra.Number (KnownNat, type (*))
import ZkFold.Symbolic.Algorithm.Hash.Blake2b (blake2b_224, blake2b_256)
import ZkFold.Symbolic.Data.ByteString (ByteString)
import ZkFold.Symbolic.Class (Symbolic)

exampleBlake2b_224 :: (KnownNat n, Symbolic c) => ByteString (8 * n) c -> ByteString 224 c
exampleBlake2b_224 = blake2b_224

exampleBlake2b_256 :: (KnownNat n, Symbolic c) => ByteString (8 * n) c -> ByteString 256 c
exampleBlake2b_256 = blake2b_256
