{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.Algebra.Pairing (specPairing) where

import           Data.Kind                              (Type)
import           Data.Typeable                          (Typeable, typeOf)
import           Foreign                                (Storable (..))
import           Prelude                                hiding (Fractional (..), Num (..), length, (^))
import           Test.Hspec
import           Test.QuickCheck                        hiding (scale)

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Algebra.EllipticCurve.BN254
import           ZkFold.Algebra.EllipticCurve.Class
import           ZkFold.Protocol.NonInteractiveProof    ()

specPairing'
    :: forall (g1 :: Type) (g2 :: Type) gt f rustg
    .  Typeable g1
    => Typeable g2
    => Typeable gt
    => Bilinear (V.Vector rustg) (PolyVec f 32) g1
    => Pairing g1 g2 gt
    => Eq g1
    => Eq g2
    => Eq gt
    => f ~ ScalarFieldOf g1
    => Field f
    => Show f
    => Arbitrary f
    => Arbitrary g1
    => Arbitrary g2
    => Show g2
    => Show g1
    => Spec
specPairing' transform = do
    describe "Elliptic curve pairing specification" $ do
        describe ("Type: " ++ show (typeOf (pairing @g1 @g2))) $ do
            describe "Pairing axioms" $ do
                it "should satisfy bilinearity" $ withMaxSuccess 10 $ do
                    property $ \(a :: f) (b :: f) p q ->
                        pairing @g1 @g2 (a `scale` p) (b `scale` q)
                            == pairing p q ^ (a * b)
                it "should satisfy non-degeneracy" $ withMaxSuccess 10 $ do
                    property $ \p q ->
                        (p /= zero && q /= zero) ==> pairing @g1 @g2 p q /= one

specPairing :: Spec
specPairing = do
    specPairing' @BN254_G1_Point @BN254_G2_Point
    specPairing' @BLS12_381_G1_Point @BLS12_381_G2_Point
