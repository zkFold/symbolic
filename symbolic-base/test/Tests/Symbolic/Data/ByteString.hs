{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.Symbolic.Data.ByteString (specByteString) where

import           Control.Monad                              (return)
import           Data.Aeson                                 (decode, encode)
import           Data.Constraint                            (withDict)
import           Data.Constraint.Nat                        (plusNat)
import           Data.Function                              (id, ($))
import           Data.Functor                               ((<$>))
import           Data.List                                  ((++))
import           GHC.Generics                               (U1)
import           Prelude                                    (show, type (~), (<>))
import qualified Prelude                                    as Haskell
import           Test.Hspec                                 (Spec, describe)
import           Test.QuickCheck                            (Property, chooseInteger, withMaxSuccess, (===))
import           Tests.Symbolic.Data.Common                 (specConstantRoundtrip, specSymbolicFunction0,
                                                             specSymbolicFunction1, specSymbolicFunction2)

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Algebra.Field                       (Zp)
import           ZkFold.Algebra.Number
import qualified ZkFold.Data.Vector                         as V
import           ZkFold.Data.Vector                         (Vector)
import           ZkFold.Symbolic.Class                      (Arithmetic)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit (ArithmeticCircuit, exec)
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators           (Iso (..), RegisterSize (..))
import           ZkFold.Symbolic.Data.UInt
import           ZkFold.Symbolic.Interpreter                (Interpreter (Interpreter))
import Tests.Common (it, toss)

type AC a = ArithmeticCircuit a U1

eval ::
  forall a n . Arithmetic a =>
  ByteString n (AC a) -> ByteString n (Interpreter a)
eval (ByteString bits) = ByteString $ Interpreter (exec bits)

type BinaryOp a = a -> a -> a

type UBinary n b = BinaryOp (ByteString n b)

isRightNeutral
    :: (KnownNat n, PrimeField (Zp p))
    => UBinary n (Interpreter (Zp p))
    -> UBinary n (AC (Zp p))
    -> ByteString n (Interpreter (Zp p))
    -> ByteString n (AC (Zp p))
    -> Natural
    -> Property
isRightNeutral f g n1 n2 x = eval (fromConstant x `g` n2) === fromConstant x `f` n1

isLeftNeutral
    :: (KnownNat n, PrimeField (Zp p))
    => UBinary n (Interpreter (Zp p))
    -> UBinary n (AC (Zp p))
    -> ByteString n (Interpreter (Zp p))
    -> ByteString n (AC (Zp p))
    -> Natural
    -> Property
isLeftNeutral f g n1 n2 x = eval (n2 `g` fromConstant x) === n1 `f` fromConstant x

testWords
    :: forall n wordSize p
    .  KnownNat n
    => KnownNat wordSize
    => Prime p
    => KnownNat (Log2 (p - 1) + 1)
    => (Div n wordSize) * wordSize ~ n
    => Spec
testWords = it ("divides a bytestring of length " <> show (value @n) <> " into words of length " <> show (value @wordSize)) $ do
    x <- toss m
    let arithBS = fromConstant x :: ByteString n (AC (Zp p))
        zpBS = fromConstant x :: ByteString n (Interpreter (Zp p))
    return (Haskell.fmap eval (toWords @(Div n wordSize) @wordSize arithBS :: Vector (Div n wordSize) (ByteString wordSize (AC (Zp p)))) === toWords @(Div n wordSize) @wordSize zpBS)
    where
        n = Haskell.toInteger $ value @n
        m = 2 Haskell.^ n

testTruncate
    :: forall n m p
    .  KnownNat n
    => PrimeField (Zp p)
    => KnownNat m
    => Spec
testTruncate = it ("truncates a bytestring of length " <> show (value @n) <> " to length " <> show (value @m)) $ do
    x <- toss m
    let arithBS = fromConstant x :: ByteString n (AC (Zp p))
        zpBS = fromConstant x :: ByteString n (Interpreter (Zp p))
    return (eval (resize arithBS :: ByteString m (AC (Zp p))) === resize zpBS)
    where
        n = Haskell.toInteger $ value @n
        m = 2 Haskell.^ n

testGrow
    :: forall n m p
    .  KnownNat n
    => PrimeField (Zp p)
    => KnownNat m
    => Resize (ByteString n (ArithmeticCircuit (Zp p) U1)) (ByteString m (ArithmeticCircuit (Zp p) U1))
    => Resize (ByteString n (Interpreter (Zp p))) (ByteString m (Interpreter (Zp p)))
    => Spec
testGrow = it ("extends a bytestring of length " <> show (value @n) <> " to length " <> show (value @m)) $ do
    x <- toss m
    let arithBS = fromConstant x :: ByteString n (AC (Zp p))
        zpBS    = fromConstant x :: ByteString n (Interpreter (Zp p))
    return (eval (resize arithBS :: ByteString m (AC (Zp p))) === resize zpBS)
    where
        n = Haskell.toInteger $ value @n
        m = 2 Haskell.^ n

testJSON :: forall n p. KnownNat n => PrimeField (Zp p) => Spec
testJSON = it "preserves the JSON invariant property" $ do
    x <- toss n
    let zpBS = fromConstant x :: ByteString n (Interpreter (Zp p))
    return $ Haskell.Just zpBS === decode (encode zpBS)
    where
        n = 2 Haskell.^ value @n

-- | For some reason, Haskell can't infer obvious type relations such as n <= n + 1...
--
specByteString'
    :: forall p n
    .  PrimeField (Zp p)
    => KnownNat n
    => (Div n n) * n ~ n
    => (Div n 4) * 4 ~ n
    => (Div n 2) * 2 ~ n
    => Spec
specByteString' = do
    let n = Haskell.fromIntegral $ value @n
        m = 2 Haskell.^ n
    describe ("ByteString" ++ show n ++ " specification") $ do
        specConstantRoundtrip @(Zp p) @(ByteString n) ("ByteString" ++ show n) "Natural" (toss m)
        specSymbolicFunction1 @(Zp p) @(ByteString n) "identity" id
        specSymbolicFunction0 @(Zp p) @(ByteString n) "true" true
        specSymbolicFunction0 @(Zp p) @(ByteString n) "false" false
        specSymbolicFunction2 @(Zp p) @(ByteString n) "bitwise OR" (||)
        specSymbolicFunction2 @(Zp p) @(ByteString n) "bitwise XOR" xor
        specSymbolicFunction2 @(Zp p) @(ByteString n) "bitwise AND" (&&)
        specSymbolicFunction1 @(Zp p) @(ByteString n) "bitwise NOT" not

        -- TODO: remove withMaxSuccess when eval is optimised
        it "applies sum modulo n via UInt correctly" $ withMaxSuccess 10 $ do
            x <- toss m
            y <- toss m

            let acX :: ByteString n (AC (Zp p)) = fromConstant x
                acY :: ByteString n (AC (Zp p)) = fromConstant y

                acSum :: ByteString n (AC (Zp p)) = from $ from acX + (from acY :: UInt n Auto (AC (Zp p)))

                zpSum :: ByteString n (Interpreter (Zp p)) = fromConstant $ x + y


            return $ eval acSum === zpSum
        it "obeys left neutrality for OR" $ isLeftNeutral @n @p (||) (||) false false <$> toss m
        it "obeys right neutrality for OR" $ isRightNeutral @n @p (||) (||) false false <$> toss m
        it "obeys left neutrality for XOR" $ isLeftNeutral @n @p xor xor false false <$> toss m
        it "obeys right neutrality for XOR" $ isRightNeutral @n @p xor xor false false <$> toss m
        it "obeys left multiplicative neutrality" $ isLeftNeutral @n @p (&&) (&&) true true <$> toss m
        it "obeys right multiplicative neutrality" $ isRightNeutral @n @p (&&) (&&) true true <$> toss m
        it "performs bit shifts correctly" $ do
            shift <- chooseInteger ((-3) * n, 3 * n)
            x <- toss m
            return $ eval @(Zp p) @n (shiftBits (fromConstant x) shift) === shiftBits (fromConstant x) shift
        it "performs bit rotations correctly" $ do
            shift <- chooseInteger ((-3) * n, 3 * n)
            x <- toss m
            return $ eval @(Zp p) @n (rotateBits (fromConstant x) shift) === rotateBits (fromConstant x) shift
        testWords @n @1 @p
        testWords @n @2 @p
        testWords @n @4 @p
        testWords @n @n @p
        it "concatenates bytestrings correctly" $ do
            x <- toss m
            y <- toss m
            z <- toss m
            let acs = fromConstant @Natural @(ByteString n (AC (Zp p))) <$> [x, y, z]
                zps = fromConstant @Natural @(ByteString n (Interpreter (Zp p))) <$> [x, y, z]
            let ac = concat @3 @n $ V.unsafeToVector @3 acs :: ByteString (3 * n) (AC (Zp p))
                zp = concat @3 @n $ V.unsafeToVector @3 zps
            return $ eval @(Zp p) @(3 * n) ac === zp
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
