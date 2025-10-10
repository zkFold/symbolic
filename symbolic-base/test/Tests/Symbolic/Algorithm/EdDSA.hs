{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Symbolic.Algorithm.EdDSA (specEdDSA) where

import Data.Function (($))
import Data.Functor ((<$>))
import GHC.Generics ((:*:) (..))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (counterexample, forAll, (.&.), (===))
import Text.Show (show)
import Prelude ((<>))

import Tests.Common (evalBool, it, toss, toss1)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (Fr)
import ZkFold.Algebra.EllipticCurve.Jubjub (Jubjub_Base, Jubjub_Scalar)
import ZkFold.Algebra.Number
import ZkFold.Symbolic.Algorithm.EdDSA (eddsaVerify)
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto), from)
import ZkFold.Algebra.EllipticCurve.Class (pointGen)
import ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point)
import qualified ZkFold.Symbolic.Data.EllipticCurve.Point.Affine as SymAffine
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA, fromUInt)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Interpreter (Interpreter)

type I = Interpreter Fr
type Point = Jubjub_Point I
type Scalar = FFA Jubjub_Scalar 'Auto I

hashToScalar :: (KnownFFA Jubjub_Base 'Auto I, KnownFFA Jubjub_Scalar 'Auto I) => Point -> Point -> FieldElement I -> Scalar
hashToScalar _ _ m = fromUInt (from m)

specEdDSA :: Spec
specEdDSA = describe "EdDSA verification (Jubjub, Poseidon Hash)" $ do
  it "verifies a correctly formed signature" $ do
    let g = pointGen @Point
        p = value @Jubjub_Scalar
    forAll (fromConstant <$> toss p) $ \(x :: Scalar) ->
      forAll (fromConstant <$> toss1 p) $ \(k :: Scalar) -> do
        let a = x `scale` g
            r = k `scale` g
            m = zero :: FieldElement I
            h = hashToScalar r a m
            s = k + h * x
            ok = eddsaVerify hashToScalar a m (r :*: s)
            rAffine = SymAffine.affinePoint r
        counterexample ("r = " <> show rAffine <> "\ns = " <> show s) $ evalBool ok === one

  it "rejects tampered signatures and degenerate inputs" $ do
    let g = pointGen @Point
        p = value @Jubjub_Scalar
    forAll (fromConstant <$> toss p) $ \(x :: Scalar) ->
      forAll (fromConstant <$> toss1 p) $ \(k :: Scalar) -> do
        let a = x `scale` g
            r = k `scale` g
            m = zero :: FieldElement I
            h = hashToScalar r a m
            s = k + h * x
            wrongS = s + one
            badWrongS = eddsaVerify hashToScalar a m (r :*: wrongS)
            badSZero = eddsaVerify hashToScalar a m (r :*: zero)
        evalBool badWrongS === zero .&. evalBool badSZero === zero


