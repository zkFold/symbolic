{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module Tests.Symbolic.Data.Int (specInt) where

import Control.Monad (return, when)
import Data.Function (id, ($))
import Data.List ((++))
import GHC.Generics (Par1 (Par1), U1)
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Gen, chooseInteger, elements, (.&.), (.||.), (===))
import Prelude (Integer, show, type (~))
import qualified Prelude as P

import Tests.Common (it)
import Tests.Symbolic.Data.Common (
  specConstantRoundtrip,
  specSymbolicFunction0,
  specSymbolicFunction1,
  specSymbolicFunction2,
 )
import ZkFold.Algebra.Class hiding (Euclidean (..))
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Compiler.ArithmeticCircuit (ArithmeticCircuit, exec, exec1)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Combinators (
  Iso (..),
  KnownRegisterSize,
  NumberOfRegisters,
  RegisterSize (..),
 )
import ZkFold.Symbolic.Data.Eq
import ZkFold.Symbolic.Data.Int
import ZkFold.Symbolic.Data.Ord
import ZkFold.Symbolic.Data.UInt (UInt (..))
import ZkFold.Symbolic.Interpreter (Interpreter (Interpreter))

tossInteger :: Natural -> Gen Integer
tossInteger (P.fromIntegral -> x) = chooseInteger (-x, x)

tossInteger1 :: Natural -> Gen Integer
tossInteger1 (P.fromIntegral -> x) = do
  p <- chooseInteger (1, x)
  n <- chooseInteger (-x, -1)
  elements [p, n]

tossPositive :: Natural -> Gen Integer
tossPositive (P.fromIntegral -> x) = chooseInteger (1, x)

type AC a = ArithmeticCircuit a U1

evalBool :: Arithmetic a => Bool (AC a) -> a
evalBool (Bool ac) = exec1 ac

evalBoolVec :: Bool (Interpreter a) -> a
evalBoolVec (Bool (Interpreter (Par1 v))) = v

execAcInt
  :: forall a n r
   . Arithmetic a
  => Int n r (AC a) -> Vector (NumberOfRegisters a n r) a
execAcInt (Int (UInt v)) = exec v

execZpInt :: forall a n r. Int n r (Interpreter a) -> Vector (NumberOfRegisters a n r) a
execZpInt (Int (UInt (Interpreter v))) = v

-- with2n :: forall n {r}. KnownNat n => (KnownNat (2 * n) => r) -> r
-- with2n = withDict (timesNat @2 @n)

specInt'
  :: forall p n r rs
   . PrimeField (Zp p)
  => KnownNat n
  => KnownRegisterSize rs
  => r ~ NumberOfRegisters (Zp p) n rs
  => KnownNat r
  => Spec
specInt' = do
  let n = value @n
      m = 2 ^ (n -! 1)
  describe ("Int" ++ show n ++ " specification") $ do
    specConstantRoundtrip @(Zp p) @(Int n rs) ("Int" ++ show n) "Integer" (tossInteger m)
    specSymbolicFunction1 @(Zp p) @(Int n rs) "identity" id
    specSymbolicFunction0 @(Zp p) @(Int n rs) "zero" zero
    specSymbolicFunction2 @(Zp p) @(Int n rs) "addition" (+)
    specSymbolicFunction1 @(Zp p) @(Int n rs) "negate" negate
    specSymbolicFunction2 @(Zp p) @(Int n rs) "subtraction" (-)
    specSymbolicFunction0 @(Zp p) @(Int n rs) "one" one
    specSymbolicFunction2 @(Zp p) @(Int n rs) "multiplication" (*)
    it "IsNegative correct" $ do
      x <- tossPositive m
      return $
        evalBoolVec @(Zp p) (isNegative @n @rs $ fromConstant x) === 0
          .&. evalBoolVec @(Zp p) (isNegative @n @rs $ fromConstant (-x)) === 1
    it "IsNegative correct-2" $ do
      x <- tossPositive m
      let t = evalBoolVec @(Zp p) (isNegative @n @rs $ fromConstant x)
      return $ (t === 0) .||. (t === 1)
    it "Abs correct" $ do
      x <- tossPositive m
      return $
        toConstant (abs (fromConstant (-x) :: Int n rs (Interpreter (Zp p)))) === x
          .&. toConstant (abs (fromConstant x :: Int n rs (Interpreter (Zp p)))) === x
          .&. toConstant (abs (zero :: Int n rs (Interpreter (Zp p)))) === 0
    it "iso uint correctly" $ do
      x <- tossInteger m
      let ix = fromConstant x :: Int n rs (AC (Zp p))
          ux = fromConstant x :: UInt n rs (AC (Zp p))
      return $ execAcInt (from ux :: Int n rs (AC (Zp p))) === execAcInt @_ @n @rs ix

    when (m > 0) $ it "performs divMod correctly" $ do
      num <- tossInteger m
      d <- tossInteger1 m
      let (acQ, acR) = (fromConstant num :: Int n rs (AC (Zp p))) `divMod` fromConstant d
          (zpQ, zpR) = (fromConstant num :: Int n rs (Interpreter (Zp p))) `divMod` fromConstant d
          (trueQ, trueR) = num `divMod` d
          refQ = execZpInt (fromConstant trueQ :: Int n rs (Interpreter (Zp p)))
          refR = execZpInt (fromConstant trueR :: Int n rs (Interpreter (Zp p)))
      return $
        (execAcInt acQ, execAcInt acR) === (execZpInt zpQ, execZpInt zpR) .&. (refQ, refR) === (execZpInt zpQ, execZpInt zpR)

    when (m > 0) $ it "performs quotRem correctly" $ do
      num <- tossInteger m
      d <- tossInteger1 m
      let (acQ, acR) = (fromConstant num :: Int n rs (AC (Zp p))) `quotRem` fromConstant d
          (zpQ, zpR) = (fromConstant num :: Int n rs (Interpreter (Zp p))) `quotRem` fromConstant d
          (trueQ, trueR) = num `P.quotRem` d
          refQ = execZpInt (fromConstant trueQ :: Int n rs (Interpreter (Zp p)))
          refR = execZpInt (fromConstant trueR :: Int n rs (Interpreter (Zp p)))
      return $
        (execAcInt acQ, execAcInt acR) === (execZpInt zpQ, execZpInt zpR) .&. (refQ, refR) === (execZpInt zpQ, execZpInt zpR)
    it "checks inequality" $ do
      x <- tossInteger m
      y <- tossInteger m

      let acInt1 = fromConstant x :: Int n rs (AC (Zp p))
          acInt2 = fromConstant y :: Int n rs (AC (Zp p))
      return $ evalBool @(Zp p) (acInt1 == acInt2) === (if x P.== y then 1 else 0)
    it "checks greater than" $ do
      x <- tossInteger m
      y <- tossInteger m
      let x' = fromConstant x :: Int n rs (Interpreter (Zp p))
          y' = fromConstant y :: Int n rs (Interpreter (Zp p))
          x'' = fromConstant x :: Int n rs (AC (Zp p))
          y'' = fromConstant y :: Int n rs (AC (Zp p))
          gt' = evalBoolVec $ x' > y'
          gt'' = evalBool @(Zp p) (x'' > y'')
          trueGt = if x P.> y then one else zero
      return $ gt' === gt'' .&. gt' === trueGt
    it "checks greater than or equal" $ do
      x <- tossInteger m
      y <- tossInteger m
      let x' = fromConstant x :: Int n rs (Interpreter (Zp p))
          y' = fromConstant y :: Int n rs (Interpreter (Zp p))
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
