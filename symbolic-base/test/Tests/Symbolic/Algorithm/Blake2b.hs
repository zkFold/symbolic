{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Symbolic.Algorithm.Blake2b where

import Crypto.Hash.BLAKE2.BLAKE2b (hash)
import qualified Data.ByteString.Internal as BI
import GHC.Exts (IsString (fromString))
import GHC.Generics hiding (from)
import Numeric.Natural (Natural)
import Test.Hspec (Spec, describe, it)
import Prelude (Eq (..), ($))

import ZkFold.Algebra.Class (FromConstant (..))
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar, Fr)
import ZkFold.Algebra.Field (Zp)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit, eval)
import qualified ZkFold.Data.Eq as ZkFold
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Algorithm.Hash.Blake2b (blake2b_224, blake2b_512)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.ByteString (ByteString (..))
import ZkFold.Symbolic.Class (Arithmetic, Symbolic)
import ZkFold.ArithmeticCircuit.Elem (compile, Elem)

blake2bNumeric :: forall c. Arithmetic c => Spec
blake2bNumeric =
  let a = blake2b_512 @0 @c $ fromConstant (0 :: Natural)
      c = hash 64 BI.empty BI.empty
   in it "computes blake2b_512 correctly on empty bytestring" $ a == fromConstant c

{-
Appendix A.  Example of BLAKE2b Computation

   We compute the unkeyed hash of three ASCII bytes "abc" with
   BLAKE2b-512 and show internal values during computation.

   BLAKE2b-512("abc") = BA 80 A5 3F 98 1C 4D 0D 6A 27 97 B6 9F 12 F6 E9
                        4C 21 2F 14 68 5A C4 B7 4B 12 BB 6F DB FF A2 D1
                        7D 87 C5 39 2A AB 79 2D C2 52 D5 DE 45 33 CC 95
                        18 D3 8A A8 DB F1 92 5A B9 23 86 ED D4 00 99 23
-}

blake2bExampleRfc :: forall c. Arithmetic c => Spec
blake2bExampleRfc =
  let abc' = blake2b_512 @3 @c $ fromConstant $ fromString @BI.ByteString "abc"
      abc = fromConstant @_ @(ByteString 512 _) $ hash 64 BI.empty "abc"
   in it "example test from rfc7693 " $ abc' == abc

equalityBlake
  :: forall c
   . Symbolic c
  => BI.ByteString
  -> ByteString 24 c
  -> Bool c
equalityBlake target input = fromConstant target ZkFold.== blake2b_224 @3 @c input

blake2bSymbolic :: Spec
blake2bSymbolic =
  let ac :: ArithmeticCircuit Fr (Vector 24) Par1
      ac = compile @Fr (:*: U1) $ equalityBlake @(Elem Fr) $ hash 28 BI.empty "abc"
      ByteString input = fromConstant @_ @(ByteString 24 Fr) $ fromString @BI.ByteString "abc"
   in it "simple test with cardano-crypto " $ eval ac input == Par1 1

specBlake2b :: Spec
specBlake2b = describe "BLAKE2b self-test validation" $ do
  blake2bNumeric @(Zp BLS12_381_Scalar)
  blake2bExampleRfc @(Zp BLS12_381_Scalar)
  blake2bSymbolic
