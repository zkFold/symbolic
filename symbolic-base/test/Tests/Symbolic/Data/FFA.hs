{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Symbolic.Data.FFA (specFFA) where

import Data.Function (($), (.))
import Data.List ((++))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Property, (===))
import Text.Show (show)
import Prelude (Integer)

import Tests.Common (it)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.EllipticCurve.Pasta (FpModulus, FqModulus)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number (Prime, value)
import ZkFold.ArithmeticCircuit.Elem (Elem, exec)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA)

type Prime256_1 = FpModulus

type Prime256_2 = FqModulus

specFFA :: Spec
specFFA = do
  specFFA' @BLS12_381_Scalar @Prime256_1
  specFFA' @BLS12_381_Scalar @Prime256_2

specFFA' :: forall p q. (Arithmetic (Zp p), Prime q, KnownFFA q (Zp p)) => Spec
specFFA' = do
  let q = value @q
  describe ("FFA " ++ show q ++ " specification") $ do
    it "FFA(Zp) embeds Zq" $ \(x :: Zp q) ->
      toConstant (fromConstant x :: FFA q (Zp p)) === x
    it "FFA(AC) embeds Zq" $ \(x :: Zp q) ->
      execAcFFA @p @q (fromConstant x) === x
    it "has zero" $ execAcFFA @p @q zero === execZpFFA @p @q zero
    it "has one" $ execAcFFA @p @q one === execZpFFA @p @q one
    it "adds correctly" $ isHom @p @q (+) (+)
    it "negates correctly" $ \(x :: Zp q) ->
      execAcFFA @p @q (negate $ fromConstant x) === execZpFFA @p @q (negate $ fromConstant x)
    it "multiplies correctly" $ isHom @p @q (*) (*)
    it "inverts correctly" $ \(x :: Zp q) ->
      execAcFFA @p @q (finv $ fromConstant x) === execZpFFA @p @q (finv $ fromConstant x)
    it "powers correctly" $ \(x :: Zp q) (e :: Integer) ->
      execAcFFA @p @q (fromConstant x ^ e) === x ^ e

execAcFFA
  :: forall p q
   . (Arithmetic (Zp p), KnownFFA q (Zp p))
  => FFA q (Elem (Zp p)) -> Zp q
execAcFFA = execZpFFA . exec

execZpFFA :: (Arithmetic (Zp p), KnownFFA q (Zp p)) => FFA q (Zp p) -> Zp q
execZpFFA = toConstant

type Binary a = a -> a -> a

type Predicate a = a -> a -> Property

isHom
  :: (Arithmetic (Zp p), KnownFFA q (Zp p))
  => Binary (FFA q (Zp p))
  -> Binary (FFA q (Elem (Zp p)))
  -> Predicate (Zp q)
isHom f g x y = execAcFFA (fromConstant x `g` fromConstant y) === execZpFFA (fromConstant x `f` fromConstant y)
