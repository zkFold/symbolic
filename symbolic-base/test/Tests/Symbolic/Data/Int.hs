{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Symbolic.Data.Int (specInt) where

import Control.Monad (return, when)
import Data.Binary (Binary)
import Data.Function (($))
import Data.List ((++))
import GHC.Generics (Par1 (Par1, unPar1))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, chooseInteger, elements, (.&.), (.||.))
import Test.QuickCheck.Instances ()
import Prelude (Integer, show)
import qualified Prelude as P

import Tests.Common (it)
import ZkFold.Algebra.Class hiding (Euclidean (..))
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number
import ZkFold.ArithmeticCircuit.Elem (Elem, exec)
import ZkFold.Data.Eq
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Int
import ZkFold.Symbolic.Data.Ord
import ZkFold.Symbolic.Data.UInt
import ZkFold.Symbolic.Testing (ringModelAxioms, (===))

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

specInt'
  :: forall p n
   . (Arithmetic (Zp p), KnownUInt n (Zp p))
  => Spec
specInt' = do
  let n = value @n
      m = 2 ^ (n -! 1)
  describe ("Int" ++ show n ++ " specification") $ do
    ringModelAxioms @(Int n) @(Zp p) arbitrary arbitrary
    it "IsNegative correct" $ do
      x <- tossPositive m
      return $
        fromBool (isNegative @n @(Zp p) $ fromConstant x) === 0
          .&. fromBool (isNegative @n @(Zp p) $ fromConstant (-x)) === 1
    it "IsNegative correct-2" $ do
      x <- tossPositive m
      let t = fromBool (isNegative @n @(Zp p) $ fromConstant x)
      return $ (t === 0) .||. (t === 1)
    it "Abs correct" $ do
      x <- tossPositive m
      return $
        toConstant (abs (fromConstant (-x) :: Int n (Zp p))) === x
          .&. toConstant (abs (fromConstant x :: Int n (Zp p))) === x
          .&. toConstant (abs (zero :: Int n (Zp p))) === 0
    it "iso uint correctly" $ do
      x <- tossInteger m
      let ix = fromConstant x :: Int n (Zp p)
          ux = fromConstant x :: UInt n (Zp p)
      return $ uintToInt ux === ix

    when (m > 0) $ it "performs divMod correctly" $ do
      num <- tossInteger m
      d <- tossInteger1 m
      let (zpQ, zpR) = (fromConstant num :: Int n (Zp p)) `divMod` fromConstant d
          (trueQ, trueR) = num `divMod` d
          refQ = fromConstant trueQ :: Int n (Zp p)
          refR = fromConstant trueR :: Int n (Zp p)
      return $ (refQ, refR) === (zpQ, zpR)

    when (m > 0) $ it "performs quotRem correctly" $ do
      num <- tossInteger m
      d <- tossInteger1 m
      let (zpQ, zpR) = (fromConstant num :: Int n (Zp p)) `quotRem` fromConstant d
          (trueQ, trueR) = num `P.quotRem` d
          refQ = fromConstant trueQ :: Int n (Zp p)
          refR = fromConstant trueR :: Int n (Zp p)
      return $ (refQ, refR) === (zpQ, zpR)
    it "checks inequality" $ do
      x <- tossInteger m
      y <- tossInteger m

      let acInt1 = fromConstant x :: Int n (Elem (Zp p))
          acInt2 = fromConstant y :: Int n (Elem (Zp p))
      return $ evalBool @(Zp p) (acInt1 == acInt2) === (if x P.== y then 1 else 0)
    it "checks greater than" $ do
      x <- tossInteger m
      y <- tossInteger m
      let x' = fromConstant x :: Int n (Zp p)
          y' = fromConstant y :: Int n (Zp p)
          x'' = fromConstant x :: Int n (Elem (Zp p))
          y'' = fromConstant y :: Int n (Elem (Zp p))
          gt' = fromBool $ x' > y'
          gt'' = evalBool @(Zp p) (x'' > y'')
          trueGt = if x P.> y then one else zero
      return $ gt' === gt'' .&. gt' === trueGt
    it "checks greater than or equal" $ do
      x <- tossInteger m
      y <- tossInteger m
      let x' = fromConstant x :: Int n (Zp p)
          y' = fromConstant y :: Int n (Zp p)
          x'' = fromConstant x :: Int n (Elem (Zp p))
          y'' = fromConstant y :: Int n (Elem (Zp p))
          ge' = fromBool $ x' >= y'
          ge1' = fromBool $ x' >= x'
          ge2' = fromBool $ y' >= y'
          ge'' = evalBool @(Zp p) (x'' >= y'')
          trueGe = if x P.>= y then one else zero
      return $ ge' === ge'' .&. ge1' === (one :: Zp p) .&. ge2' === (one :: Zp p) .&. ge' === trueGe

specInt :: Spec
specInt = do
  specInt' @BLS12_381_Scalar @64
  specInt' @BLS12_381_Scalar @64
