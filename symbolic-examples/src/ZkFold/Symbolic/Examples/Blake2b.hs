{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module ZkFold.Symbolic.Examples.Blake2b (exampleBlake2b_224, exampleBlake2b_256) where

import Data.Function ((.))
import ZkFold.Algebra.Number (KnownNat, type (*))
import ZkFold.Symbolic.Algorithm.Hash.Blake2b (blake2b_224, blake2b_256)
import ZkFold.Symbolic.Compat (CompatData (..))
import ZkFold.Symbolic.Data.ByteString (ByteString)
import ZkFold.Symbolic.V2 (Symbolic)

exampleBlake2b_224
  :: (KnownNat n, Symbolic c)
  => CompatData (ByteString (8 * n)) c -> CompatData (ByteString 224) c
exampleBlake2b_224 = CompatData . blake2b_224 . compatData

exampleBlake2b_256
  :: (KnownNat n, Symbolic c)
  => CompatData (ByteString (8 * n)) c -> CompatData (ByteString 256) c
exampleBlake2b_256 = CompatData . blake2b_256 . compatData
