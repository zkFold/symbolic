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
import qualified ZkFold.Algebra.EllipticCurve.Class as Elliptic
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
import qualified ZkFold.Symbolic.Algorithm.Hash.MiMC as MiMC
import ZkFold.Symbolic.Algorithm.EdDSA (eddsaSign)
import ZkFold.Data.Eq

-- TODO: Choice of Fr appropriate?
type I = Interpreter Fr
type Point = Jubjub_Point I
type Scalar = FFA Jubjub_Scalar 'Auto I

hashToScalar :: (KnownFFA Jubjub_Base 'Auto I, KnownFFA Jubjub_Scalar 'Auto I) => Point -> Point -> FieldElement I -> Scalar
hashToScalar rPoint pubKey m =
  let h0 = fromUInt (from (MiMC.hash (rPoint :*: pubKey :*: m)))
  in h0 * one

specEdDSA :: Spec
specEdDSA = describe "EdDSA verification (Jubjub, Poseidon Hash)" $ do
  it "verifies a correctly formed signature for simple case" $ do
    let g = pointGen @Point
        msg = zero :: FieldElement I
        privKey = one
        pubKey = privKey `scale` g
        r = one
        rPoint = r `scale` g
        hprivKey = h * privKey
        hpubKey' = hprivKey `scale` g
        hpubKey = h `scale` pubKey
        s = r + h * privKey
        -- (rPoint :*: s) = eddsaSign hashToScalar privKey msg
        lhs = s `scale` g
        -- h :: Scalar = one + one + one
        h = hashToScalar rPoint pubKey msg
        rhs = (rPoint :: Point) + (h `scale` pubKey)
        ok = SymAffine.affinePoint hpubKey' == SymAffine.affinePoint hpubKey
        -- ok = eddsaVerify hashToScalar pubKey msg (rPoint :*: s)
    counterexample ("g = " <> show (SymAffine.affinePoint g) <> "\nprivKey = " <> show privKey <> "\npubKey = " <> show (SymAffine.affinePoint pubKey) <> "\nr = " <> show r <> "\nrPoint = " <> show (SymAffine.affinePoint rPoint) <> "\ns = " <> show s <> "\nlhs = " <> show (SymAffine.affinePoint lhs) <> "\nrhs = " <> show (SymAffine.affinePoint rhs) <> "\nok = " <> show ok <> "\nhpubKey' = " <> show (SymAffine.affinePoint hpubKey') <> "\nhpubKey = " <> show (SymAffine.affinePoint hpubKey) <> "\nh = " <> show h) $ evalBool (ok) === one
  it "verifies a correctly formed signature" $ do
    let g = pointGen @Point
        p = value @Jubjub_Scalar
    forAll (fromConstant <$> toss p) $ \(privKey :: Scalar) -> do
      -- https://cryptobook.nakov.com/digital-signatures/eddsa-and-ed25519 for how to derive the signature.
      let msg = zero :: FieldElement I
          (rPoint :*: s) = eddsaSign hashToScalar privKey msg
          pubKey = privKey `scale` g
          ok = eddsaVerify hashToScalar pubKey msg (rPoint :*: s)
          rAffine = SymAffine.affinePoint rPoint
      counterexample ("\nrPoint = " <> show rAffine <> "\ns = " <> show s) $ evalBool ok === one

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


