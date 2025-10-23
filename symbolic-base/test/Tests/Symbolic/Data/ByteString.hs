{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}

module Tests.Symbolic.Data.ByteString (specByteString) where

import Control.Monad (return)
import Data.Aeson (decode, encode)
import Data.Bits (shift, (.&.), (.|.))
import qualified Data.Bits as Bits
import Data.Constraint (withDict)
import Data.Constraint.Nat (plusNat)
import Data.Function (flip, ($))
import Data.Functor ((<$>))
import Data.List ((++))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Property, arbitrary, chooseInteger)
import Prelude (show, (<>), type (~))
import qualified Prelude as Haskell

import Tests.Common (it, toss)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number
import ZkFold.ArithmeticCircuit.Elem (Elem, exec)
import ZkFold.Data.Eq ((==))
import ZkFold.Data.Vector (Vector)
import qualified ZkFold.Data.Vector as V
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Testing

type BinaryOp a = a -> a -> a

type UBinary n b = BinaryOp (ByteString n b)

isRightNeutral
  :: (KnownNat n, PrimeField (Zp p), 3 <= p)
  => UBinary n (Zp p)
  -> UBinary n (Elem (Zp p))
  -> ByteString n (Zp p)
  -> ByteString n (Elem (Zp p))
  -> Natural
  -> Property
isRightNeutral f g n1 n2 x =
  fromBool (exec (fromConstant x `g` n2) == fromConstant x `f` n1) === one

isLeftNeutral
  :: (KnownNat n, PrimeField (Zp p), 3 <= p)
  => UBinary n (Zp p)
  -> UBinary n (Elem (Zp p))
  -> ByteString n (Zp p)
  -> ByteString n (Elem (Zp p))
  -> Natural
  -> Property
isLeftNeutral f g n1 n2 x =
  fromBool (exec (n2 `g` fromConstant x) == n1 `f` fromConstant x) === one

testWords
  :: forall n wordSize p
   . KnownNat n
  => KnownNat wordSize
  => (Prime p, 3 <= p)
  => KnownNat (Log2 (p - 1) + 1)
  => (Div n wordSize) * wordSize ~ n
  => Spec
testWords = it ("divides a bytestring of length " <> show (value @n) <> " into words of length " <> show (value @wordSize)) $ do
  x <- toss m
  let arithBS = fromConstant x :: ByteString n (Elem (Zp p))
      zpBS = fromConstant x :: ByteString n (Zp p)
  return $
    fromBool
      ( Haskell.fmap
          exec
          (toWords @(Div n wordSize) @wordSize arithBS :: Vector (Div n wordSize) (ByteString wordSize (Elem (Zp p))))
          == toWords @(Div n wordSize) @wordSize zpBS
      )
      === one
 where
  n = Haskell.toInteger $ value @n
  m = 2 Haskell.^ n

testTruncate
  :: forall n m p
   . KnownNat n
  => (PrimeField (Zp p), 3 <= p)
  => KnownNat m
  => Spec
testTruncate = it ("truncates a bytestring of length " <> show (value @n) <> " to length " <> show (value @m)) $ do
  x <- toss m
  let arithBS = fromConstant x :: ByteString n (Elem (Zp p))
      zpBS = fromConstant x :: ByteString n (Zp p)
  return $ fromBool (exec (resize arithBS :: ByteString m (Elem (Zp p))) == resize zpBS) === one
 where
  n = Haskell.toInteger $ value @n
  m = 2 Haskell.^ n

testGrow
  :: forall n m p
   . KnownNat n
  => (PrimeField (Zp p), 3 <= p)
  => KnownNat m
  => Spec
testGrow = it ("extends a bytestring of length " <> show (value @n) <> " to length " <> show (value @m)) $ do
  x <- toss m
  let arithBS = fromConstant x :: ByteString n (Elem (Zp p))
      zpBS = fromConstant x :: ByteString n (Zp p)
  return $ fromBool (exec (resize arithBS :: ByteString m (Elem (Zp p))) == resize zpBS) === one
 where
  n = Haskell.toInteger $ value @n
  m = 2 Haskell.^ n

testJSON :: forall n p. (KnownNat n, 3 <= p) => PrimeField (Zp p) => Spec
testJSON = it "preserves the JSON invariant property" $ do
  x <- toss n
  let zpBS = fromConstant x :: ByteString n (Zp p)
  return $ fromBool (Haskell.Just zpBS == decode (encode zpBS)) === one
 where
  n = 2 Haskell.^ value @n

-- | For some reason, Haskell can't infer obvious type relations such as n <= n + 1...
specByteString'
  :: forall p n
   . PrimeField (Zp p)
  => (KnownNat n, 3 <= p)
  => (Div n n) * n ~ n
  => (Div n 4) * 4 ~ n
  => (Div n 2) * 2 ~ n
  => Spec
specByteString' = do
  let n = Haskell.fromIntegral $ value @n
      m = 2 Haskell.^ n
  describe ("ByteString" ++ show n ++ " specification") $ do
    it "embedding" $ embedding @(ByteString n) @(Zp p) arbitrary (toss m)
    it "true" $ commutative0 @(ByteString n (Zp p)) (m -! 1) true
    it "false" $ commutative0 @(ByteString n (Zp p)) 0 false
    it "bitwise OR" $ commutative2 @(ByteString n (Zp p)) (.|.) (||) arbitrary
    it "bitwise XOR" $ commutative2 @(ByteString n (Zp p)) Bits.xor xor arbitrary
    it "bitwise AND" $ commutative2 @(ByteString n (Zp p)) (.&.) (&&) arbitrary
    it "bitwise NOT" $ commutative1 @(ByteString n (Zp p)) (Bits.xor (m -! 1)) not arbitrary

    it "obeys left neutrality for OR" $ isLeftNeutral @n @p (||) (||) false false <$> toss m
    it "obeys right neutrality for OR" $ isRightNeutral @n @p (||) (||) false false <$> toss m
    it "obeys left neutrality for XOR" $ isLeftNeutral @n @p xor xor false false <$> toss m
    it "obeys right neutrality for XOR" $ isRightNeutral @n @p xor xor false false <$> toss m
    it "obeys left multiplicative neutrality" $ isLeftNeutral @n @p (&&) (&&) true true <$> toss m
    it "obeys right multiplicative neutrality" $ isRightNeutral @n @p (&&) (&&) true true <$> toss m
    it "performs bit shifts correctly" $
      commutativePar
        (\(Haskell.fromEnum -> s) x -> Bits.shift x s .&. (m -! 1))
        (flip $ shiftBits @(ByteString n (Zp p)))
        do
          s <- chooseInteger ((-3) * n, 3 * n)
          (s,) <$> arbitrary
    it "performs bit rotations correctly" $
      let n' = Haskell.fromEnum n
          forw r = r `Haskell.rem` n'
          compl r =
            if r Haskell.> 0
              then (r `Haskell.mod` n') Haskell.- n'
              else r `Haskell.mod` n'
       in commutativePar
            ( \(Haskell.fromEnum -> r) nat ->
                ((nat `shift` forw r) .|. (nat `shift` compl r)) .&. (m -! 1)
            )
            (flip $ rotateBits @(ByteString n (Zp p)))
            do
              rot <- chooseInteger ((-3) * n, 3 * n)
              (rot,) <$> arbitrary
    testWords @n @1 @p
    testWords @n @2 @p
    testWords @n @4 @p
    testWords @n @n @p
    it "concatenates bytestrings correctly" $ do
      x <- toss m
      y <- toss m
      z <- toss m
      let acs = fromConstant @Natural @(ByteString n (Elem (Zp p))) <$> [x, y, z]
          zps = fromConstant @Natural @(ByteString n (Zp p)) <$> [x, y, z]
      let ac = concat @3 @n $ V.unsafeToVector @3 acs :: ByteString (3 * n) (Elem (Zp p))
          zp = concat @3 @n $ V.unsafeToVector @3 zps
      return $ exec @(Zp p) @(ByteString (3 * n)) ac === zp
    testTruncate @n @1 @p
    testTruncate @n @4 @p
    testTruncate @n @8 @p
    testTruncate @n @16 @p
    testTruncate @n @32 @p
    testTruncate @n @n @p
    withDict (plusNat @n @1) (testGrow @n @(n + 1) @p)
    withDict (plusNat @n @10) (testGrow @n @(n + 10) @p)
    withDict (plusNat @n @128) (testGrow @n @(n + 128) @p)
    withDict (plusNat @n @n) (testGrow @n @(n + n) @p)
    testJSON @n @p

specByteString :: Spec
specByteString = do
  specByteString' @BLS12_381_Scalar @32
  specByteString' @BLS12_381_Scalar @512
  specByteString' @BLS12_381_Scalar @508 -- Twice the number of bits encoded by BLS12_381_Scalar.
