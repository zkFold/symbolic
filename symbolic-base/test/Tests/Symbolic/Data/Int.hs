{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}


module Tests.Symbolic.Data.Int (specInt) where


import           Control.Applicative                         ((<*>))
import           Control.Monad                               (return, when)
import           Data.Binary                                 (Binary)
import           Data.Function                               (($))
import           Data.Functor                                ((<$>))
import           Data.List                                   ((++))
import           GHC.Generics                                (Par1 (Par1), U1)
import           Prelude                                     (Integer, show, type (~))
import qualified Prelude                                     as P
import           Test.Hspec                                  (Spec, describe)
import           Test.QuickCheck                             (Gen, Property, chooseInteger, elements, (.&.), (.||.),
                                                              (===))
import           Tests.Symbolic.ArithmeticCircuit            (exec1, it)

import           ZkFold.Base.Algebra.Basic.Class             hiding (Euclidean (..))
import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Data.Vector                     (Vector)
import           ZkFold.Symbolic.Class                       (Arithmetic)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit, exec)
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Combinators            (Iso (..), KnownRegisterSize, NumberOfRegisters,
                                                              RegisterSize (..))
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.Int
import           ZkFold.Symbolic.Data.Ord
import           ZkFold.Symbolic.Data.UInt                   (UInt (..))
import           ZkFold.Symbolic.Interpreter                 (Interpreter (Interpreter))

toss :: Natural -> Gen Integer
toss (P.fromIntegral -> x) = chooseInteger (-x, x)

toss1 :: Natural -> Gen Integer
toss1 (P.fromIntegral -> x) = do
    p <- chooseInteger (1, x)
    n <- chooseInteger (-x, -1)
    elements [p, n]

tossp :: Natural -> Gen Integer
tossp (P.fromIntegral -> x) = chooseInteger (1, x)

type AC a = ArithmeticCircuit a U1 U1

evalBool :: forall a . (Arithmetic a, Binary a) => Bool (AC a) -> a
evalBool (Bool ac) = exec1 ac

evalBoolVec :: forall a . Bool (Interpreter a) -> a
evalBoolVec (Bool (Interpreter (Par1 v))) = v

execAcInt ::
  forall a n r . (Arithmetic a, Binary a) =>
  Int n r (AC a) -> Vector (NumberOfRegisters a n r) a
execAcInt (Int (UInt v)) = exec v

execZpInt :: forall a n r . Int n r (Interpreter a) -> Vector (NumberOfRegisters a n r) a
execZpInt (Int (UInt (Interpreter v))) = v

type BinaryOp a = a -> a -> a

type UBinary n b r = BinaryOp (Int n b r)

isHom
    :: forall n p r
    .  KnownNat n
    => PrimeField (Zp p)
    => KnownRegisterSize r
    => UBinary n r (Interpreter (Zp p))
    -> UBinary n r (AC (Zp p))
    -> BinaryOp Integer
    -> Integer
    -> Integer
    -> Property
isHom f g h x y = execAcInt (fromConstant x `g` fromConstant y) === execZpInt (fromConstant x `f` fromConstant y)
              .&. execZpInt (fromConstant x `f` fromConstant y) === execZpInt @(Zp p) @n @r (fromConstant $ x `h` y)

-- with2n :: forall n {r}. KnownNat n => (KnownNat (2 * n) => r) -> r
-- with2n = withDict (timesNat @2 @n)

specInt'
    :: forall p n r rs
    .  PrimeField (Zp p)
    => KnownNat n
    => KnownRegisterSize rs
    => r ~ NumberOfRegisters (Zp p) n rs
    => KnownNat r
    => Spec
specInt' = do
    let n = value @n
        m = 2 ^ (n -! 1)
    describe ("Int" ++ show n ++ " specification") $ do
        it "Zp embeds Integer" $ do
            x <- toss m
            return $ toConstant @(Int n rs (Interpreter (Zp p))) (fromConstant x) === x
        it "Integer embeds Zp" $ \(x :: Int n rs (Interpreter (Zp p))) ->
            fromConstant (toConstant x) === x
        it "IsNegative correct" $ do
            x <- tossp m
            return $ evalBoolVec @(Zp p) (isNegative @n @rs $ fromConstant x) === 0 .&.
                     evalBoolVec @(Zp p) (isNegative @n @rs $ fromConstant (- x)) === 1
        it "IsNegative correct-2" $ do
            x <- toss m
            let t = evalBoolVec @(Zp p) (isNegative @n @rs $ fromConstant x)
            return $ (t === 0) .||. (t === 1)
        it "Abs correct" $ do
            x <- tossp m
            return $ toConstant (abs (fromConstant (-x) :: Int n rs (Interpreter (Zp p)))) === x .&.
                    toConstant (abs (fromConstant x :: Int n rs (Interpreter (Zp p)))) === x .&.
                    toConstant (abs (zero :: Int n rs (Interpreter (Zp p)))) === 0
        it "AC embeds Integer" $ do
            x <- toss m
            return $ execAcInt @(Zp p) @n @rs (fromConstant x) === execZpInt @_ @n @rs (fromConstant x)
        it "adds correctly" $ isHom @n @p @rs (+) (+) (+) <$> toss m <*> toss m
        it "has zero" $ execAcInt @(Zp p) @n @rs zero === execZpInt @_ @n @rs zero
        it "negates correctly" $ do
            x <- toss m
            return $ execAcInt @(Zp p) @n @rs (negate (fromConstant x)) === execZpInt @_ @n @rs (negate (fromConstant x))
        it "multiplies correctly" $ isHom @n @p @rs (*) (*) (*) <$> toss m <*> toss m
        it "subtracts correctly" $ isHom @n @p @rs (-) (-) (-) <$> toss m <*> toss m
        it "iso uint correctly" $ do
            x <- toss m
            let ix = fromConstant x :: Int n rs (AC (Zp p))
                ux = fromConstant x :: UInt n rs (AC (Zp p))
            return $ execAcInt (from ux :: Int n rs (AC (Zp p))) === execAcInt @_ @n @rs ix

        when (m > 0) $ it "performs divMod correctly" $ do
            num <- toss m
            d <- toss1 m
            let (acQ, acR) = (fromConstant num :: Int n rs (AC (Zp p))) `divMod` fromConstant d
                (zpQ, zpR) = (fromConstant num :: Int n rs (Interpreter (Zp p))) `divMod` fromConstant d
                (trueQ, trueR) = num `divMod` d
                refQ = execZpInt (fromConstant trueQ ::Int n rs (Interpreter (Zp p)))
                refR = execZpInt (fromConstant trueR ::Int n rs (Interpreter (Zp p)))
            return $ (execAcInt acQ, execAcInt acR) === (execZpInt zpQ, execZpInt zpR) .&. (refQ, refR) === (execZpInt zpQ, execZpInt zpR)

        when (m > 0) $ it "performs quotRem correctly" $ do
            num <- toss m
            d <- toss1 m
            let (acQ, acR) = (fromConstant num :: Int n rs (AC (Zp p))) `quotRem` fromConstant d
                (zpQ, zpR) = (fromConstant num :: Int n rs (Interpreter (Zp p))) `quotRem` fromConstant d
                (trueQ, trueR) = num `P.quotRem` d
                refQ = execZpInt (fromConstant trueQ ::Int n rs (Interpreter (Zp p)))
                refR = execZpInt (fromConstant trueR ::Int n rs (Interpreter (Zp p)))
            return $ (execAcInt acQ, execAcInt acR) === (execZpInt zpQ, execZpInt zpR) .&. (refQ, refR) === (execZpInt zpQ, execZpInt zpR)
        it "checks inequality" $ do
            x <- toss m
            y <- toss m

            let acInt1 = fromConstant x :: Int n rs (AC (Zp p))
                acInt2 = fromConstant y :: Int n rs (AC (Zp p))
            return $ evalBool @(Zp p) (acInt1 == acInt2) === (if x P.== y then 1 else 0)
        it "checks greater than" $ do
            x <- toss m
            y <- toss m
            let x' = fromConstant x  :: Int n rs (Interpreter (Zp p))
                y' = fromConstant y  :: Int n rs (Interpreter (Zp p))
                x'' = fromConstant x :: Int n rs (AC (Zp p))
                y'' = fromConstant y :: Int n rs (AC (Zp p))
                gt' = evalBoolVec $ x' > y'
                gt'' = evalBool @(Zp p) (x'' > y'')
                trueGt = if x P.> y then one else zero
            return $ gt' === gt'' .&. gt' === trueGt
        it "checks greater than or equal" $ do
            x <- toss m
            y <- toss m
            let x' = fromConstant x  :: Int n rs (Interpreter (Zp p))
                y' = fromConstant y  :: Int n rs (Interpreter (Zp p))
                x'' = fromConstant x :: Int n rs (AC (Zp p))
                y'' = fromConstant y :: Int n rs (AC (Zp p))
                ge' = evalBoolVec $ x' >= y'
                ge1' = evalBoolVec $ x' >= x'
                ge2' = evalBoolVec $ y' >= y'
                ge'' = evalBool @(Zp p) (x'' >= y'')
                trueGe = if x P.>= y then one else zero
            return $ ge' === ge'' .&. ge1' === (one :: Zp p) .&. ge2' === (one :: Zp p) .&. ge' === trueGe

specInt :: Spec
specInt = do
    specInt' @BLS12_381_Scalar @64 @_ @Auto
    specInt' @BLS12_381_Scalar @64 @_ @(Fixed 10)
