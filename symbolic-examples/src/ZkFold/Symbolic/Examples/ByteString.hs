{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Symbolic.Examples.ByteString (
  exampleByteStringAnd,
  exampleByteStringOr,
  exampleByteStringResize,
  exampleByteStringAdd,
  exampleSHA,
) where

import Data.Function (($), (.))
import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Symbolic.Algorithm.Hash.SHA2 (SHA2, sha2)
import ZkFold.Symbolic.Compat (CompatContext, CompatData (..))
import ZkFold.Symbolic.Data.Bool (BoolType (..))
import ZkFold.Symbolic.Data.ByteString (ByteString)
import ZkFold.Symbolic.Data.Combinators (Iso (..), RegisterSize (..), Resize (..))
import ZkFold.Symbolic.Data.UInt (UInt)
import ZkFold.Symbolic.V2 (Symbolic)

exampleByteStringAnd
  :: (KnownNat n, Symbolic c)
  => CompatData (ByteString n) c
  -> CompatData (ByteString n) c
  -> CompatData (ByteString n) c
exampleByteStringAnd = (&&)

exampleByteStringOr
  :: (KnownNat n, Symbolic c)
  => CompatData (ByteString n) c
  -> CompatData (ByteString n) c
  -> CompatData (ByteString n) c
exampleByteStringOr = (||)

exampleByteStringResize
  :: (KnownNat n, KnownNat k, Symbolic c)
  => CompatData (ByteString n) c -> CompatData (ByteString k) c
exampleByteStringResize = CompatData . resize . compatData

exampleByteStringAdd
  :: forall n c
   . (KnownNat n, Symbolic c)
  => CompatData (ByteString n) c
  -> CompatData (ByteString n) c
  -> CompatData (ByteString n) c
exampleByteStringAdd (CompatData x) (CompatData y) =
  CompatData $ from (from x + from y :: UInt n Auto (CompatContext c))

exampleSHA
  :: forall n c
   . SHA2 "SHA256" (CompatContext c) n
  => CompatData (ByteString n) c -> CompatData (ByteString 256) c
exampleSHA = CompatData . sha2 @"SHA256" . compatData
