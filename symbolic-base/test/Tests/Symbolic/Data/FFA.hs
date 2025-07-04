{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Symbolic.Data.FFA (specFFA) where

import Data.Function (($))
import Data.List ((++))
import GHC.Generics (U1)
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
import ZkFold.ArithmeticCircuit (ArithmeticCircuit, exec)
import ZkFold.Symbolic.Data.Combinators (KnownRegisterSize (..), RegisterSize (..))
import ZkFold.Symbolic.Data.FFA (FFA (FFA), KnownFFA)
import ZkFold.Symbolic.Data.FieldElement (FieldElement (FieldElement))
import ZkFold.Symbolic.Data.UInt (UInt (..))
import ZkFold.Symbolic.Interpreter (Interpreter (Interpreter))

type Prime256_1 = FpModulus

type Prime256_2 = FqModulus

specFFA :: Spec
specFFA = do
  specFFA' @BLS12_381_Scalar @Prime256_1 @Auto
  specFFA' @BLS12_381_Scalar @Prime256_2 @Auto
  specFFA' @BLS12_381_Scalar @Prime256_1 @(Fixed 16)
  specFFA' @BLS12_381_Scalar @Prime256_2 @(Fixed 16)

specFFA' :: forall p q r. (PrimeField (Zp p), Prime q, KnownFFA q r (Interpreter (Zp p))) => Spec
specFFA' = do
  let q = value @q
  let r = regSize @r
  describe ("FFA " ++ show q ++ " " ++ show r ++ " specification") $ do
    it "FFA(Zp) embeds Zq" $ \(x :: Zp q) ->
      toConstant (fromConstant x :: FFA q r (Interpreter (Zp p))) === x
    it "FFA(AC) embeds Zq" $ \(x :: Zp q) ->
      execAcFFA @p @q @r (fromConstant x) === x
    it "has zero" $ execAcFFA @p @q @r zero === execZpFFA @p @q @r zero
    it "has one" $ execAcFFA @p @q @r one === execZpFFA @p @q @r one
    it "adds correctly" $ isHom @p @q @r (+) (+)
    it "negates correctly" $ \(x :: Zp q) ->
      execAcFFA @p @q @r (negate $ fromConstant x) === execZpFFA @p @q @r (negate $ fromConstant x)
    it "multiplies correctly" $ isHom @p @q @r (*) (*)
    it "inverts correctly" $ \(x :: Zp q) ->
      execAcFFA @p @q @r (finv $ fromConstant x) === execZpFFA @p @q @r (finv $ fromConstant x)
    it "powers correctly" $ \(x :: Zp q) (e :: Integer) ->
      execAcFFA @p @q @r (fromConstant x ^ e) === x ^ e

execAcFFA
  :: forall p q r
   . (PrimeField (Zp p), KnownFFA q r (Interpreter (Zp p)))
  => FFA q r (ArithmeticCircuit (Zp p) U1) -> Zp q
execAcFFA (FFA (FieldElement nv) (UInt uv)) =
  execZpFFA $
    FFA
      (FieldElement $ Interpreter $ exec nv)
      (UInt @_ @r $ Interpreter $ exec uv)

execZpFFA
  :: (PrimeField (Zp p), KnownFFA q r (Interpreter (Zp p)))
  => FFA q r (Interpreter (Zp p)) -> Zp q
execZpFFA = toConstant

type Binary a = a -> a -> a

type Predicate a = a -> a -> Property

isHom
  :: (PrimeField (Zp p), KnownFFA q r (Interpreter (Zp p)))
  => Binary (FFA q r (Interpreter (Zp p)))
  -> Binary (FFA q r (ArithmeticCircuit (Zp p) U1))
  -> Predicate (Zp q)
isHom f g x y = execAcFFA (fromConstant x `g` fromConstant y) === execZpFFA (fromConstant x `f` fromConstant y)
