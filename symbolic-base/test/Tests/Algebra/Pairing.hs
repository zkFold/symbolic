{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.Algebra.Pairing (specPairing) where

import           Data.Kind                                   (Type)
import           Data.Typeable                               (Typeable, typeOf)
import qualified Data.Vector                                 as V
import           Prelude                                     hiding (Fractional (..), Num (..), length, (^))
import           Test.Hspec
import           Test.QuickCheck                             hiding (scale)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Algebra.EllipticCurve.BN254
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.Polynomials.Univariate  (Poly, PolyVec, deg, evalPolyVec, polyVecConstant,
                                                              polyVecDiv, toPolyVec, vec2poly)
import           ZkFold.Base.Protocol.NonInteractiveProof    ()

propVerificationKZG
    :: forall g1 g2 gt f
    .  Pairing g1 g2 gt
    => Eq gt
    => f ~ ScalarFieldOf g1
    => f ~ ScalarFieldOf g2
    => Field f
    => Eq f
    => f -> PolyVec f 32 -> f -> Bool
propVerificationKZG x p z =
    let n  = deg @f @(Poly f) $ vec2poly p

        -- G1
        gs = V.fromList $ map ((`scale` pointGen) . (x^)) [0 .. n]
        g0 = V.head gs :: g1

        -- G2
        h0 = pointGen :: g2
        h1 = x `scale` h0

        com = msm
        -- Proving a polynomial evaluation
        pz = p `evalPolyVec` z
        h  = (p - polyVecConstant pz) `polyVecDiv` toPolyVec [negate z, one]
        w  = gs `com` h
        v0 = gs `com` p - (pz `scale` g0) + z `scale` w

        -- Verification
    in pairing v0 h0 == pairing w h1

specPairing'
    :: forall (g1 :: Type) (g2 :: Type) gt f
    .  Typeable g1
    => Typeable g2
    => Typeable gt
    => Pairing g1 g2 gt
    => Eq g1
    => Eq g2
    => Eq gt
    => f ~ ScalarFieldOf g1
    => Field f
    => Eq f
    => Show f
    => Arbitrary f
    => Arbitrary g1
    => Arbitrary g2
    => Show g2
    => Show g1
    => Spec
specPairing' = do
    describe "Elliptic curve pairing specification (SLOW)" $ do
        describe ("Type: " ++ show (typeOf (pairing @g1 @g2))) $ do
            describe "Pairing axioms" $ do
                it "should satisfy bilinearity" $ withMaxSuccess 10 $ do
                    property $ \(a :: f) (b :: f) p q ->
                        pairing @g1 @g2 (a `scale` p) (b `scale` q)
                            == pairing p q ^ (a * b)
                it "should satisfy non-degeneracy" $ withMaxSuccess 10 $ do
                    property $ \p q ->
                        (p /= zero && q /= zero) ==> pairing @g1 @g2 p q /= one
            describe "Pairing verification" $ do
                it "should verify KZG commitments" $ withMaxSuccess 10 $ do
                    property $ propVerificationKZG @g1 @g2 @gt @f

specPairing :: Spec
specPairing = do
    specPairing' @BN254_G1_Point @BN254_G2_Point
    specPairing' @BLS12_381_G1_Point @BLS12_381_G2_Point
