{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -freduction-depth=0 #-} -- Avoid reduction overflow error caused by NumberOfRegisters

module Tests.Symbolic.Data.UInt (specUInt) where

import           Control.Applicative                         ((<*>))
import           Control.Monad                               (return, when)
import           Data.Aeson                                  (decode, encode)
import           Data.Binary                                 (Binary)
import           Data.Constraint
import           Data.Constraint.Nat                         (timesNat)
import           Data.Function                               (($))
import           Data.Functor                                ((<$>))
import           Data.List                                   ((++))
import           GHC.Generics                                (Par1 (Par1), U1)
import           Prelude                                     (show, type (~))
import qualified Prelude                                     as P
import           Test.Hspec                                  (Spec, describe)
import           Test.QuickCheck                             (Gen, Property, withMaxSuccess, (.&.), (===))
import           Tests.Symbolic.ArithmeticCircuit            (exec1, it)

import           ZkFold.Base.Algebra.Basic.Class             hiding (Euclidean (..))
import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Data.Vector                     (Vector)
import           ZkFold.Prelude                              (chooseNatural)
import           ZkFold.Symbolic.Class                       (Arithmetic)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit, exec)
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators            (Ceil, GetRegisterSize, Iso (..), KnownRegisterSize,
                                                              NumberOfRegisters, RegisterSize (..))
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.Ord
import           ZkFold.Symbolic.Data.UInt
import           ZkFold.Symbolic.Interpreter                 (Interpreter (Interpreter))

toss :: Natural -> Gen Natural
toss x = chooseNatural (0, x)

toss1 :: Natural -> Gen Natural
toss1 x = chooseNatural (1, x)

type AC a = ArithmeticCircuit a U1 U1

evalBool :: forall a . (Arithmetic a, Binary a) => Bool (AC a) -> a
evalBool (Bool ac) = exec1 ac

evalBoolVec :: forall a . Bool (Interpreter a) -> a
evalBoolVec (Bool (Interpreter (Par1 v))) = v

evalBS ::
  forall a n . (Arithmetic a, Binary a) =>
  ByteString n (AC a) -> ByteString n (Interpreter a)
evalBS (ByteString bits) = ByteString $ Interpreter (exec bits)

execAcUint ::
  forall a n r . (Arithmetic a, Binary a) =>
  UInt n r (AC a) -> Vector (NumberOfRegisters a n r) a
execAcUint (UInt v) = exec v

execZpUint :: forall a n r . UInt n r (Interpreter a) -> Vector (NumberOfRegisters a n r) a
execZpUint (UInt (Interpreter v)) = v

overflowSub :: forall n . KnownNat n => BinaryOp Natural
overflowSub x y
  | x > y = x -! y
  | P.otherwise = 2 ^ value @n + x -! y

type BinaryOp a = a -> a -> a

type UBinary n b r = BinaryOp (UInt n b r)

isHom
    :: forall n p r
    .  KnownNat n
    => PrimeField (Zp p)
    => KnownRegisterSize r
    => UBinary n r (Interpreter (Zp p))
    -> UBinary n r (AC (Zp p))
    -> BinaryOp Natural
    -> Natural
    -> Natural
    -> Property
isHom f g h x y = execAcUint (fromConstant x `g` fromConstant y) === execZpUint (fromConstant x `f` fromConstant y)
              .&. execZpUint (fromConstant x `f` fromConstant y) === execZpUint @(Zp p) @n @r (fromConstant $ x `h` y)

with2n :: forall n {r}. KnownNat n => (KnownNat (2 * n) => r) -> r
with2n = withDict (timesNat @2 @n)

specUInt'
    :: forall p n r r2n rs
    .  PrimeField (Zp p)
    => KnownNat n
    => KnownRegisterSize rs
    => r ~ NumberOfRegisters (Zp p) n rs
    => r2n ~ NumberOfRegisters (Zp p) (2 * n) rs
    => KnownNat r
    => KnownNat r2n
    => KnownNat (Ceil (GetRegisterSize (Zp p) n rs) OrdWord)
    => KnownNat (Ceil (GetRegisterSize (Zp p) (2 * n) rs) OrdWord)
    => Spec
specUInt' = do
    let n = value @n
        m = 2 ^ n -! 1
    describe ("UInt" ++ show n ++ " specification") $ do
        it "Zp embeds Integer" $ do
            x <- toss m
            return $ toConstant @(UInt n rs (Interpreter (Zp p))) (fromConstant x) === x
        it "Integer embeds Zp" $ \(x :: UInt n rs (Interpreter (Zp p))) ->
            fromConstant (toConstant x) === x
        it "AC embeds Integer" $ do
            x <- toss m
            return $ execAcUint @(Zp p) @n @rs (fromConstant x) === execZpUint @_ @n @rs (fromConstant x)
        it "adds correctly" $ isHom @n @p @rs (+) (+) (+) <$> toss m <*> toss m
        it "has zero" $ execAcUint @(Zp p) @n @rs zero === execZpUint @_ @n @rs zero
        it "negates correctly" $ do
            x <- toss m
            return $ execAcUint @(Zp p) @n @rs (negate (fromConstant x)) === execZpUint @_ @n @rs (negate (fromConstant x))
        it "multiplies correctly" $ isHom @n @p @rs (*) (*) (*) <$> toss m <*> toss m
        it "subtracts correctly" $ isHom @n @p @rs (-) (-) (overflowSub @n) <$> toss m <*> toss m
        it "iso uint correctly" $ do
            x <- toss m
            let bx = fromConstant x :: ByteString n (AC (Zp p))
                ux = fromConstant x :: UInt n rs (AC (Zp p))
            return $ execAcUint (from bx :: UInt n rs (AC (Zp p))) === execAcUint ux
        it "iso bytestring correctly" $ do
            x <- toss m
            let ux = fromConstant x :: UInt n Auto (AC (Zp p))
                bx = fromConstant x :: ByteString n (AC (Zp p))
            return $ evalBS (from ux :: ByteString n (AC (Zp p))) === evalBS bx

        when (m > 0) $ it "performs divMod correctly" $ do
            num <- toss m
            d <- toss1 m
            let (acQ, acR) = (fromConstant num :: UInt n rs (AC (Zp p))) `divMod` fromConstant d
                (zpQ, zpR) = (fromConstant num :: UInt n rs (Interpreter (Zp p))) `divMod` fromConstant d
                (trueQ, trueR) = num `divMod` d
                refQ = execZpUint (fromConstant trueQ ::UInt n rs (Interpreter (Zp p)))
                refR = execZpUint (fromConstant trueR ::UInt n rs (Interpreter (Zp p)))
            return $ (execAcUint acQ, execAcUint acR) === (execZpUint zpQ, execZpUint zpR) .&. (refQ, refR) === (execZpUint zpQ, execZpUint zpR)

        it "performs productMod correctly" $ do
            a <- toss  (2 ^ ((n `div` 2) -! 2))
            b <- toss  (2 ^ ((n `div` 2) -! 2))
            d <- toss1 (2 ^ ((n `div` 2) -! 2))
            let (acQ, acR) = (productMod (fromConstant a :: UInt n rs (AC (Zp p))) (fromConstant b) (fromConstant d))
                (zpQ, zpR) = (productMod (fromConstant a :: UInt n rs (Interpreter (Zp p))) (fromConstant b) (fromConstant d))
                (trueQ, trueR) = (a * b) `divMod` d
                refQ = execZpUint (fromConstant trueQ ::UInt n rs (Interpreter (Zp p)))
                refR = execZpUint (fromConstant trueR ::UInt n rs (Interpreter (Zp p)))
            return $ (execAcUint acQ, execAcUint acR) === (execZpUint zpQ, execZpUint zpR) .&. (refQ, refR) === (execZpUint zpQ, execZpUint zpR)

        when (n <= 128) $ it "calculates gcd correctly" $ withMaxSuccess 10 $ do
            x <- toss m
            y <- toss m
            let (_, _, r) = eea (fromConstant x :: UInt n rs (Interpreter (Zp p))) (fromConstant y)
                ans = fromConstant (P.gcd x y) :: UInt n rs (Interpreter (Zp p))
            return $ r === ans
        when (n <= 128) $ it "calculates Bezout coefficients correctly" $ withMaxSuccess 10 $ do
            x' <- toss m
            y' <- toss m
            let x = x' `P.div` P.gcd x' y'
                y = y' `P.div` P.gcd x' y'

                -- We will test Bezout coefficients by multiplying two UInts less than 2^n, hence we need 2^(2n) bits to store the result
                zpX = with2n @n (fromConstant x) :: UInt (2 * n) rs (Interpreter (Zp p))
                zpY = with2n @n (fromConstant y)
                (s, t, _) = with2n @n (eea zpX zpY)
            -- if x and y are coprime, s is the multiplicative inverse of x modulo y and t is the multiplicative inverse of y modulo x
            return $ with2n @n ((zpX * s) `mod` zpY === one) .&. with2n @n ((zpY * t) `mod` zpX === one)
        it "has one" $ execAcUint @(Zp p) @n @rs one === execZpUint @_ @n @rs one
        it "strictly adds correctly" $ do
            x <- toss m
            isHom @n @p @rs strictAdd strictAdd (+) x <$> toss (m -! x)
        it "strictly subtracts correctly" $ do
            x <- toss m
            isHom @n @p @rs strictSub strictSub (-!) x <$> toss x
        it "strictly multiplies correctly" $ do
            x <- toss m
            isHom @n @p @rs strictMul strictMul (*) x <$> toss (m `P.div` x)

        it "extends correctly" $ do
            x <- toss m
            let acUint =  with2n @n (fromConstant x) :: UInt n rs (AC (Zp p))
                zpUint =  with2n @n (fromConstant x) :: UInt (2 * n) rs (Interpreter (Zp p))
            return $ execAcUint @(Zp p) (with2n @n (resize acUint :: UInt (2 * n) rs (AC (Zp p)))) === execZpUint zpUint

        it "shrinks correctly" $ do
            x <- toss (m * m)
            let acUint = with2n @n (fromConstant x) :: UInt (2 * n) rs (AC (Zp p))
                zpUint = fromConstant x :: UInt n rs (Interpreter (Zp p))
            return $ execAcUint @(Zp p) (with2n @n (resize acUint :: UInt n rs (AC (Zp p)))) === execZpUint zpUint

        it "checks equality" $ do
            x <- toss m
            let acUint = fromConstant x :: UInt n rs (AC (Zp p))
            return $ evalBool @(Zp p) (acUint == acUint) === one

        it "checks inequality" $ do
            x <- toss m
            y' <- toss m
            let y = if y' P.== x then (x + 1) `mod` (2 ^ n) else y'

            let acUint1 = fromConstant x :: UInt n rs (AC (Zp p))
                acUint2 = fromConstant y :: UInt n rs (AC (Zp p))

            return $ evalBool @(Zp p) (acUint1 == acUint2) === (if x == y then 1 else 0)

        it "checks greater than" $ do
            x <- toss m
            y <- toss m
            let x' = fromConstant x  :: UInt n rs (Interpreter (Zp p))
                y' = fromConstant y  :: UInt n rs (Interpreter (Zp p))
                x'' = fromConstant x :: UInt n rs (AC (Zp p))
                y'' = fromConstant y :: UInt n rs (AC (Zp p))
                gt' = evalBoolVec $ x' > y'
                gt'' = evalBool @(Zp p) (x'' > y'')
                trueGt = if x > y then one else zero
            return $ gt' === gt'' .&. gt' === trueGt
        it "checks greater than or equal" $ do
            x <- toss m
            y <- toss m
            let x' = fromConstant x  :: UInt n rs (Interpreter (Zp p))
                y' = fromConstant y  :: UInt n rs (Interpreter (Zp p))
                x'' = fromConstant x :: UInt n rs (AC (Zp p))
                y'' = fromConstant y :: UInt n rs (AC (Zp p))
                ge' = evalBoolVec $ x' >= y'
                ge1' = evalBoolVec $ x' >= x'
                ge2' = evalBoolVec $ y' >= y'
                ge'' = evalBool @(Zp p) (x'' >= y'')
                trueGe = if x >= y then one else zero
            return $ ge' === ge'' .&. ge1' === (one :: Zp p) .&. ge2' === (one :: Zp p) .&. ge' === trueGe
        when (m > 0) $ it "Raises to power correctly" $ withMaxSuccess 10 $ do
            num <- toss m
            modulus <- toss1 m
            p <- toss 255
            let nI = fromConstant num :: UInt n rs (Interpreter (Zp p))
                mI = fromConstant modulus :: UInt n rs (Interpreter (Zp p))
                pI = fromConstant p :: UInt 8 rs (Interpreter (Zp p))

                rI = execZpUint $ with2n @n $ expMod nI pI mI
                rTrue = execZpUint (fromConstant ((num^p) `mod` modulus) :: UInt n rs (Interpreter (Zp p)))
            return $ rI === rTrue
        it "preserves the JSON invariant property" $ do
            x <- toss m
            let x' = fromConstant x :: UInt n rs (Interpreter (Zp p))
            return $ P.Just x' === decode (encode x)

specUInt :: Spec
specUInt = do
    specUInt' @BLS12_381_Scalar @0 @_ @_ @Auto
    specUInt' @BLS12_381_Scalar @32 @_ @_ @Auto
    specUInt' @BLS12_381_Scalar @500 @_ @_ @Auto

    specUInt' @BLS12_381_Scalar @0 @_ @_ @(Fixed 10)
    specUInt' @BLS12_381_Scalar @32 @_ @_ @(Fixed 10)
    specUInt' @BLS12_381_Scalar @500 @_ @_ @(Fixed 10)
