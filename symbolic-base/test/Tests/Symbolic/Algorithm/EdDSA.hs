{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Symbolic.Algorithm.EdDSA (specEdDSA) where

import Data.Function (($))
import Data.Functor ((<$>))
import GHC.Generics ((:*:) (..))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (counterexample, forAll, (.&.), (===))
import Text.Show (show)
import Prelude ((<>))

import Tests.Common (evalBool, it, toss)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (Fr)
import ZkFold.Algebra.EllipticCurve.Class (pointGen)
import ZkFold.Algebra.EllipticCurve.Jubjub (Jubjub_Scalar)
import ZkFold.Algebra.Number
import ZkFold.Symbolic.Algorithm.EdDSA (eddsaSign, eddsaVerify)
import qualified ZkFold.Symbolic.Algorithm.Hash.MiMC as MiMC
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto))
import ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point)
import qualified ZkFold.Symbolic.Data.EllipticCurve.Point.Affine as SymAffine
import ZkFold.Symbolic.Data.FFA (FFA)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Interpreter (Interpreter)

-- TODO: Choice of Fr appropriate?
type I = Interpreter Fr

type Point = Jubjub_Point I

type Scalar = FFA Jubjub_Scalar 'Auto I

specEdDSA :: Spec
specEdDSA = describe "EdDSA verification (Jubjub, Poseidon Hash)" $ do
  it "verifies a correctly formed signature, and denies tampered signatures" $ do
    let g = pointGen @Point
        p = value @Jubjub_Scalar
    forAll (fromConstant <$> toss p) $ \(privKey :: Scalar) -> do
      let msg = zero :: FieldElement I
          (rPoint :*: s) = eddsaSign MiMC.hash privKey msg -- TODO: Use Poseidon hash in this module.
          pubKey = privKey `scale` g
          ok = eddsaVerify MiMC.hash pubKey msg (rPoint :*: s)
          rAffine = SymAffine.affinePoint rPoint
      counterexample ("\nrPoint = " <> show rAffine <> "\ns = " <> show s) $
        evalBool ok === one
          .&. evalBool (eddsaVerify MiMC.hash pubKey msg (rPoint :*: (s + one))) === zero
