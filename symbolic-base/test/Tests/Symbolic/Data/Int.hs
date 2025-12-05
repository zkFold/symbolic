{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module Tests.Symbolic.Data.Int (specInt) where

import Control.Monad (return, when)
import Data.Binary (Binary)
import Data.Function (id, ($))
import Data.List ((++))
import GHC.Generics (Par1 (Par1, unPar1))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Gen, chooseInteger, elements, (.&.), (.||.), (===))
import Prelude (Integer, show, type (~))
import qualified Prelude as P

import Tests.Common (it)
import Tests.Symbolic.Data.Common
import ZkFold.Algebra.Class hiding (Euclidean (..))
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number
import ZkFold.ArithmeticCircuit.Elem (Elem, exec)
import ZkFold.Data.Eq
import ZkFold.Data.Iso
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Int
import ZkFold.Symbolic.Data.Ord
import ZkFold.Symbolic.Data.UInt

tossInteger :: Natural -> Gen Integer
tossInteger (P.fromIntegral -> x) = chooseInteger (-x, x)

tossInteger1 :: Natural -> Gen Integer
tossInteger1 (P.fromIntegral -> x) = do
  p <- chooseInteger (1, x)
  n <- chooseInteger (-x, -1)
  elements [p, n]

tossPositive :: Natural -> Gen Integer
tossPositive (P.fromIntegral -> x) = chooseInteger (1, x)

evalBool :: (Arithmetic a, Binary a) => Bool (Elem a) -> a
evalBool (Bool ac) = unPar1 $ exec (Par1 ac)

execAcInt
  :: forall a n r
   . (Arithmetic a, Binary a)
  => Int n r (Elem a) -> Vector (NumberOfRegisters a n r) a
execAcInt (Int (UInt v)) = exec v

execZpInt :: forall a n r. Int n r a -> Vector (NumberOfRegisters a n r) a
execZpInt (Int (UInt v)) = v

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
        fromBool (isNegative @n @rs @(Zp p) $ fromConstant x) === 0
          .&. fromBool (isNegative @n @rs @(Zp p) $ fromConstant (-x)) === 1
    it "IsNegative correct-2" $ do
      x <- tossPositive m
      let t = fromBool (isNegative @n @rs @(Zp p) $ fromConstant x)
      return $ (t === 0) .||. (t === 1)
    it "Abs correct" $ do
      x <- tossPositive m
      return $
        toConstant (abs (fromConstant (-x) :: Int n rs (Zp p))) === x
          .&. toConstant (abs (fromConstant x :: Int n rs (Zp p))) === x
          .&. toConstant (abs (zero :: Int n rs (Zp p))) === 0
    it "iso uint correctly" $ do
      x <- tossInteger m
      let ix = fromConstant x :: Int n rs (Elem (Zp p))
          ux = fromConstant x :: UInt n rs (Elem (Zp p))
      return $ execAcInt (from ux :: Int n rs (Elem (Zp p))) === execAcInt @_ @n @rs ix

    when (m > 0) $ it "performs divMod correctly" $ do
      num <- tossInteger m
      d <- tossInteger1 m
      let (acQ, acR) = (fromConstant num :: Int n rs (Elem (Zp p))) `divMod` fromConstant d
          (zpQ, zpR) = (fromConstant num :: Int n rs (Zp p)) `divMod` fromConstant d
          (trueQ, trueR) = num `divMod` d
          refQ = execZpInt (fromConstant trueQ :: Int n rs (Zp p))
          refR = execZpInt (fromConstant trueR :: Int n rs (Zp p))
      return $
        (execAcInt acQ, execAcInt acR) === (execZpInt zpQ, execZpInt zpR) .&. (refQ, refR) === (execZpInt zpQ, execZpInt zpR)

    when (m > 0) $ it "performs quotRem correctly" $ do
      num <- tossInteger m
      d <- tossInteger1 m
      let (acQ, acR) = (fromConstant num :: Int n rs (Elem (Zp p))) `quotRem` fromConstant d
          (zpQ, zpR) = (fromConstant num :: Int n rs (Zp p)) `quotRem` fromConstant d
          (trueQ, trueR) = num `P.quotRem` d
          refQ = execZpInt (fromConstant trueQ :: Int n rs (Zp p))
          refR = execZpInt (fromConstant trueR :: Int n rs (Zp p))
      return $
        (execAcInt acQ, execAcInt acR) === (execZpInt zpQ, execZpInt zpR) .&. (refQ, refR) === (execZpInt zpQ, execZpInt zpR)
    it "checks inequality" $ do
      x <- tossInteger m
      y <- tossInteger m

      let acInt1 = fromConstant x :: Int n rs (Elem (Zp p))
          acInt2 = fromConstant y :: Int n rs (Elem (Zp p))
      return $ evalBool @(Zp p) (acInt1 == acInt2) === (if x P.== y then 1 else 0)
    it "checks greater than" $ do
      x <- tossInteger m
      y <- tossInteger m
      let x' = fromConstant x :: Int n rs (Zp p)
          y' = fromConstant y :: Int n rs (Zp p)
          x'' = fromConstant x :: Int n rs (Elem (Zp p))
          y'' = fromConstant y :: Int n rs (Elem (Zp p))
          gt' = fromBool $ x' > y'
          gt'' = evalBool @(Zp p) (x'' > y'')
          trueGt = if x P.> y then one else zero
      return $ gt' === gt'' .&. gt' === trueGt
    it "checks greater than or equal" $ do
      x <- tossInteger m
      y <- tossInteger m
      let x' = fromConstant x :: Int n rs (Zp p)
          y' = fromConstant y :: Int n rs (Zp p)
          x'' = fromConstant x :: Int n rs (Elem (Zp p))
          y'' = fromConstant y :: Int n rs (Elem (Zp p))
          ge' = fromBool $ x' >= y'
          ge1' = fromBool $ x' >= x'
          ge2' = fromBool $ y' >= y'
          ge'' = evalBool @(Zp p) (x'' >= y'')
          trueGe = if x P.>= y then one else zero
      return $ ge' === ge'' .&. ge1' === (one :: Zp p) .&. ge2' === (one :: Zp p) .&. ge' === trueGe

specInt :: Spec
specInt = do
  specInt' @BLS12_381_Scalar @64 @_ @Auto
  specInt' @BLS12_381_Scalar @64 @_ @(Fixed 10)
