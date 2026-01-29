{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Tests.Symbolic.Data.UInt (specUInt) where

import Control.Applicative (pure)
import Control.Monad (return, when)
import Data.Aeson (decode, encode)
import Data.Binary (Binary)
import Data.Function (($))
import Data.List ((++))
import Data.Maybe (Maybe (..))
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..))
import GHC.Generics (Par1 (..))
import GHC.TypeNats (sameNat)
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Arbitrary (arbitrary), withMaxSuccess, (.&.))
import Test.QuickCheck.Instances ()
import Prelude (show)
import qualified Prelude as P

import Tests.Common (it, toss, toss1)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number
import ZkFold.ArithmeticCircuit.Elem (Elem, exec)
import ZkFold.Data.Eq
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString hiding (resize)
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.Ord
import ZkFold.Symbolic.Data.UInt
import ZkFold.Symbolic.Testing (commutative2, ringModelAxioms, (===))

evalBool :: (Arithmetic a, Binary a) => Bool (Elem a) -> a
evalBool (Bool ac) = unPar1 $ exec (Par1 ac)

type BitsOf p = NumberOfBits (Zp p)

specUInt'
  :: forall p n
   . (Arithmetic (Zp p), KnownUInt 8 (Zp p))
  => (KnownUInt n (Zp p), KnownUInt (2 * n) (Zp p))
  => Spec
specUInt' = do
  ringModelAxioms @(UInt n) @(Zp p) arbitrary arbitrary
  let n = value @n
      m = 2 ^ n
  describe ("UInt " ++ show n ++ " extra operations specification") do
    it "strictly adds correctly" $ commutative2 @(UInt n (Zp p)) (+) (+!) do
      x <- toss m
      y <- toss (m -! x)
      return (fromConstant x, fromConstant y)
    it "strictly subtracts correctly" $
      commutative2 @(UInt n (Zp p)) (-) (-!) do
        x <- toss m
        y <- toss (x + 1)
        return (fromConstant x, fromConstant y)
    it "strictly multiplies correctly" $
      commutative2 @(UInt n (Zp p)) (*) (*!) do
        x <- toss m
        y <- toss (if x == 0 then m else m `div` x)
        return (fromConstant x, fromConstant y)
    it "iso uint correctly" \(x :: Zp (2 ^ n)) ->
      beBSToUInt @n @(Zp p) (fromConstant $ toConstant x) === fromConstant x
    it "iso bytestring correctly" \(x :: Zp (2 ^ n)) ->
      uintToBSbe @n @(Zp p) (fromConstant x) === fromConstant (toConstant x)
    case sameNat (Proxy @n) $ Proxy @(BitsOf p) of
      Just Refl -> it "iso fieldelement correctly" \(x :: Zp p) ->
        let ux = fromConstant (toConstant x) :: UInt (BitsOf p) (Zp p)
            fx = fromConstant x :: FieldElement (Zp p)
         in uintToFE ux === fx
      Nothing -> pure ()
    -- TODO: remove withMaxSuccess when eval is optimised
    it "sums bytestrings modulo n correctly" $ withMaxSuccess 10 $ do
      x <- toss m
      y <- toss m
      let acX :: ByteString n (Elem (Zp p)) = fromConstant x
          acY :: ByteString n (Elem (Zp p)) = fromConstant y
          acSum :: ByteString n (Elem (Zp p)) =
            uintToBSbe $ beBSToUInt acX + beBSToUInt acY
          zpSum :: ByteString n (Zp p) = fromConstant $ x + y
      return $ exec acSum === zpSum

    when (m > 1) $ it "performs divMod correctly" $ do
      num <- toss m
      d <- toss1 m
      let (acQ, acR) = (fromConstant num :: UInt n (Elem (Zp p))) `divMod` fromConstant d
          (zpQ, zpR) = (fromConstant num :: UInt n (Zp p)) `divMod` fromConstant d
          (trueQ, trueR) = num `divMod` d
          refQ = fromConstant trueQ :: UInt n (Zp p)
          refR = fromConstant trueR :: UInt n (Zp p)
      return $
        (exec acQ, exec acR) === (zpQ, zpR)
          .&. (refQ, refR) === (zpQ, zpR)

    when (n <= 128) $ it "calculates gcd correctly" $ withMaxSuccess 10 $ do
      x <- toss m
      y <- toss m
      let (_, _, r) = eea (fromConstant x :: UInt n (Zp p)) (fromConstant y)
          ans = fromConstant (P.gcd x y) :: UInt n (Zp p)
      return $ r === ans
    when (n <= 128) $ it "calculates Bezout coefficients correctly" $ withMaxSuccess 10 $ do
      x' <- toss m
      y' <- toss m
      let x = x' `P.div` P.gcd x' y'
          y = y' `P.div` P.gcd x' y'

          -- We will test Bezout coefficients by multiplying two UInts less than 2^n, hence we need 2^(2n) bits to store the result
          zpX = fromConstant x :: UInt (2 * n) (Zp p)
          zpY = fromConstant y
          (s, t, _) = eea zpX zpY
      -- if x and y are coprime, s is the multiplicative inverse of x modulo y and t is the multiplicative inverse of y modulo x
      return $ ((zpX * s) `mod` zpY === one) .&. ((zpY * t) `mod` zpX === one)

    it "extends correctly" $ do
      x <- toss m
      let acUint = fromConstant x :: UInt n (Zp p)
          zpUint = fromConstant x :: UInt (2 * n) (Zp p)
      return $ resizeUInt acUint === zpUint

    it "shrinks correctly" $ do
      x <- toss (m * m)
      let acUint = fromConstant x :: UInt (2 * n) (Zp p)
          zpUint = fromConstant x :: UInt n (Zp p)
      return $ resizeUInt acUint === zpUint

    it "checks equality" $ do
      x <- toss m
      let acUint = fromConstant x :: UInt n (Elem (Zp p))
      return $ evalBool @(Zp p) (acUint == acUint) === one

    it "checks inequality" $ do
      x <- toss m
      y' <- toss m
      let y = if y' P.== x then (x + 1) `mod` (2 ^ n) else y'

      let acUint1 = fromConstant x :: UInt n (Elem (Zp p))
          acUint2 = fromConstant y :: UInt n (Elem (Zp p))

      return $ evalBool @(Zp p) (acUint1 == acUint2) === (if x == y then 1 else 0)

    it "checks greater than" $ do
      x <- toss m
      y <- toss m
      let x' = fromConstant x :: UInt n (Zp p)
          y' = fromConstant y :: UInt n (Zp p)
          x'' = fromConstant x :: UInt n (Elem (Zp p))
          y'' = fromConstant y :: UInt n (Elem (Zp p))
          gt' = fromBool $ x' > y'
          gt'' = evalBool @(Zp p) (x'' > y'')
          trueGt = if x > y then one else zero
      return $ gt' === gt'' .&. gt' === trueGt
    it "checks greater than or equal" $ do
      x <- toss m
      y <- toss m
      let x' = fromConstant x :: UInt n (Zp p)
          y' = fromConstant y :: UInt n (Zp p)
          x'' = fromConstant x :: UInt n (Elem (Zp p))
          y'' = fromConstant y :: UInt n (Elem (Zp p))
          ge' = fromBool $ x' >= y'
          ge1' = fromBool $ x' >= x'
          ge2' = fromBool $ y' >= y'
          ge'' = evalBool @(Zp p) (x'' >= y'')
          trueGe = if x >= y then one else zero
      return $ ge' === ge'' .&. ge1' === (one :: Zp p) .&. ge2' === (one :: Zp p) .&. ge' === trueGe
    when (m > 0) $ it "Raises to power correctly" $ withMaxSuccess 10 $ do
      num <- toss m
      modulus <- toss1 m
      p <- toss 255
      let nI = fromConstant num :: UInt n (Zp p)
          mI = fromConstant modulus :: UInt n (Zp p)
          pI = fromConstant p :: UInt 8 (Zp p)

          rI = expMod nI pI mI
          rTrue = fromConstant ((num ^ p) `mod` modulus) :: UInt n (Zp p)
      return $ rI === rTrue
    it "preserves the JSON invariant property" $ do
      x <- toss m
      let x' = fromConstant x :: UInt n (Zp p)
      return $ P.Just x' === decode (encode x)

specUInt :: Spec
specUInt = do
  specUInt' @BLS12_381_Scalar @0
  specUInt' @BLS12_381_Scalar @32
  specUInt' @BLS12_381_Scalar @(BitsOf BLS12_381_Scalar)
  specUInt' @BLS12_381_Scalar @500
