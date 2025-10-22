{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Symbolic.Algorithm.EdDSA (specEdDSA) where

import Data.Function (($))
import Data.Functor ((<$>))
import GHC.Generics ((:*:) (..))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (counterexample, forAll, (.&.), (===))
import Text.Show (show)
import Prelude ((<>))

import Tests.Common (it, toss)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (pointGen)
import ZkFold.Algebra.EllipticCurve.Jubjub (Fq, Jubjub_Scalar, Jubjub_Base)
import ZkFold.Algebra.Number
import ZkFold.Data.Bool (BoolType (..))
import ZkFold.Symbolic.Algorithm.EdDSA (eddsaSign, eddsaVerify)
import qualified ZkFold.Symbolic.Algorithm.Hash.MiMC as MiMC
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto))
import ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point)
import qualified ZkFold.Symbolic.Data.EllipticCurve.Point.Affine as SymAffine
import ZkFold.Symbolic.Data.FFA (FFA)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Interpreter (Interpreter)

type I = Interpreter Fq

type Point = Jubjub_Point I

type Scalar = FFA Jubjub_Scalar 'Auto I

specEdDSA :: Spec
-- TODO: (#729) We need to shift to Poseidon hash once the bug in Poseidon hash is fixed.
specEdDSA = describe "EdDSA verification (Jubjub, MiMC Hash)" $ do
  it "verifies a correctly formed signature, and denies tampered signatures" $ do
    let g = pointGen @Point
    forAll (fromConstant <$> toss (value @Jubjub_Scalar)) $ \(privKey :: Scalar) -> do
      forAll (fromConstant <$> toss (value @Jubjub_Base)) $ \(msg :: FieldElement I) -> do
        let (rPoint :*: s) = eddsaSign MiMC.hash privKey msg
            pubKey = privKey `scale` g
            ok = eddsaVerify MiMC.hash pubKey msg (rPoint :*: s)
            rAffine = SymAffine.affinePoint rPoint
        counterexample ("\nrPoint = " <> show rAffine <> "\ns = " <> show s) $
          ok === true
            .&. eddsaVerify MiMC.hash pubKey msg (rPoint :*: (s + one)) === false
