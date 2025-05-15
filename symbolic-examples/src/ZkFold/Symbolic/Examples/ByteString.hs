{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Symbolic.Examples.ByteString (
    exampleByteStringAnd,
    exampleByteStringOr,
    exampleByteStringResize,
    exampleByteStringAdd,
    exampleSHA
  ) where

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Number               (KnownNat)
import           ZkFold.Symbolic.Algorithm.Hash.SHA2 (SHA2, sha2)
import           ZkFold.Symbolic.Class               (Symbolic)
import           ZkFold.Symbolic.Data.Bool           (BoolType (..))
import           ZkFold.Symbolic.Data.ByteString     (ByteString)
import           ZkFold.Symbolic.Data.Combinators    (Iso (..), RegisterSize (..), Resize (..))
import           ZkFold.Symbolic.Data.UInt           (UInt)

exampleByteStringAnd ::
  (KnownNat n, Symbolic c) => ByteString n c -> ByteString n c -> ByteString n c
exampleByteStringAnd = (&&)

exampleByteStringOr ::
  (KnownNat n, Symbolic c) => ByteString n c -> ByteString n c -> ByteString n c
exampleByteStringOr = (||)

exampleByteStringResize ::
  (KnownNat n, KnownNat k, Symbolic c) =>
  ByteString n c -> ByteString k c
exampleByteStringResize = resize

exampleByteStringAdd ::
  forall n c. (KnownNat n, Symbolic c) => ByteString n c -> ByteString n c -> ByteString n c
exampleByteStringAdd x y = from (from x + from y :: UInt n Auto c)

exampleSHA :: forall n c.
  SHA2 "SHA256" c n
  => ByteString n c -> ByteString 256 c
exampleSHA = sha2 @"SHA256"
