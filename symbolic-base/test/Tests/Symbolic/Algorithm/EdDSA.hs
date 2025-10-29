module Tests.Symbolic.Algorithm.EdDSA (specEdDSA) where

import Data.Function (($), (.))
import Data.Functor ((<$>))
import GHC.Generics ((:*:) (..))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (counterexample, forAll, (.&.), (===))
import Text.Show (show)
import Prelude ((<>))

import Tests.Common (it, toss)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (pointGen)
import ZkFold.Algebra.EllipticCurve.Jubjub (Fq, Jubjub_Base, Jubjub_Scalar)
import ZkFold.Algebra.Number
import ZkFold.Data.Bool (BoolType (..))
import ZkFold.Symbolic.Algorithm.EdDSA (eddsaSign, eddsaVerify)
import qualified ZkFold.Symbolic.Algorithm.Hash.MiMC as MiMC
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto))
import ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point)
import qualified ZkFold.Symbolic.Data.EllipticCurve.Point.Affine as SymAffine
import ZkFold.Symbolic.Data.FFA (FFA)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Compat (CompatData (CompatData), CompatContext (..))
import ZkFold.Symbolic.Data.V2 (SymbolicData(toLayout), Layout)
import ZkFold.Symbolic.Data.Vec (Vec(Vec))
import ZkFold.Symbolic.Data.Class (LayoutFunctor)

type Point = Jubjub_Point Fq

type Scalar = FFA Jubjub_Scalar 'Auto Fq

hasher :: (SymbolicData f, LayoutFunctor (Layout f Fq)) => f Fq -> CompatData FieldElement Fq
hasher = CompatData . MiMC.hash . Vec . CompatContext . toLayout

specEdDSA :: Spec
-- TODO: (#729) We need to shift to Poseidon hash once the bug in Poseidon hash is fixed.
specEdDSA = describe "EdDSA verification (Jubjub, MiMC Hash)" $ do
  it "verifies a correctly formed signature, and denies tampered signatures" $ do
    let g = pointGen @Point
    forAll (fromConstant <$> toss (value @Jubjub_Scalar)) $ \(privKey :: Scalar) -> do
      forAll (fromConstant <$> toss (value @Jubjub_Base)) $ \(msg :: CompatData FieldElement Fq) -> do
        let (rPoint :*: s) = eddsaSign hasher privKey msg
            pubKey = privKey `scale` g
            rAffine = SymAffine.affinePoint rPoint
        counterexample ("\nrPoint = " <> show rAffine <> "\ns = " <> show s) $
          eddsaVerify hasher pubKey msg (rPoint :*: s) === true
            .&. eddsaVerify hasher pubKey msg (rPoint :*: (s + one)) === false
            .&. eddsaVerify hasher pubKey msg (((one + one :: Scalar) `scale` rPoint) :*: s) === false
            .&. eddsaVerify hasher pubKey (msg + one) (rPoint :*: s) === false
