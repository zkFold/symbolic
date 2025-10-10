{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Symbolic.Algorithm.EdDSA (specEdDSA) where

import Data.Function (($))
import GHC.Generics ((:*:) (..))
import Test.Hspec (Spec, describe)
import Test.QuickCheck ((.&.), (===))
import Prelude ()

import Tests.Common (evalBool, it)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (Fr)
import ZkFold.Algebra.EllipticCurve.Jubjub (Jubjub_Base, Jubjub_Scalar)
import ZkFold.Algebra.Number
import ZkFold.Symbolic.Algorithm.EdDSA (eddsaVerify)
import qualified ZkFold.Symbolic.Algorithm.Hash.Poseidon as Poseidon
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto), from)
import ZkFold.Algebra.EllipticCurve.Class (pointGen)
import ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point)
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA, fromUInt)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Interpreter (Interpreter)

type I = Interpreter Fr
type Point = Jubjub_Point I
type Scalar = FFA Jubjub_Scalar 'Auto I

hashToScalar :: (KnownFFA Jubjub_Base 'Auto I, KnownFFA Jubjub_Scalar 'Auto I) => Point -> Point -> FieldElement I -> Scalar
hashToScalar r a m = fromUInt (from (Poseidon.hash (r :*: a :*: m)))

specEdDSA :: Spec
specEdDSA = describe "EdDSA verification (Jubjub, Poseidon H)" $ do
  it "verifies a correctly formed signature" $ do
    let g = pointGen @Point
        x = fromConstant (7 :: Natural) :: Scalar -- private key
        k = fromConstant (11 :: Natural) :: Scalar -- nonce
        a = x `scale` g
        r = k `scale` g
        m = zero :: FieldElement I
        h = hashToScalar r a m
        s = k + h * x
        ok = eddsaVerify hashToScalar a m (r :*: s)
    evalBool ok === one

  it "rejects tampered signatures and degenerate inputs" $ do
    let g = pointGen @Point
        x = fromConstant (5 :: Natural) :: Scalar
        k = fromConstant (3 :: Natural) :: Scalar
        a = x `scale` g
        r = k `scale` g
        m = zero :: FieldElement I
        h = hashToScalar r a m
        s = k + h * x
        wrongS = s + one
        badWrongS = eddsaVerify hashToScalar a m (r :*: wrongS)
        badSZero = eddsaVerify hashToScalar a m (r :*: zero)
    evalBool badWrongS === zero .&. evalBool badSZero === zero


