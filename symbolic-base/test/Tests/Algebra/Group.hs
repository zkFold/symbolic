{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use -"                  #-}

module Tests.Algebra.Group (specGroup) where

import           Data.Data                                   (Typeable, typeOf)
import           Prelude                                     hiding (Fractional (..), Num (..), length)
import           Test.Hspec
import           Test.QuickCheck

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Algebra.EllipticCurve.BN254
import           ZkFold.Base.Algebra.EllipticCurve.Ed25519   (Ed25519_Point)
import           ZkFold.Base.Algebra.EllipticCurve.Pasta     (Pallas_Point, Vesta_Point)
import           ZkFold.Base.Algebra.EllipticCurve.PlutoEris (Eris_Point, Pluto_Point)
import           ZkFold.Base.Algebra.EllipticCurve.Secp256k1 (Secp256k1_Point)

specGroup' :: forall a . (AdditiveGroup a, Eq a, Show a, Arbitrary a, Typeable a) => Spec
specGroup' = do
    describe "Group specification" $ do
        describe ("Type: " ++ show (typeOf @a zero)) $ do
            describe "Additive group axioms" $ do
                it "should satisfy additive associativity" $ do
                    property $ \(a :: a) b c -> (a + b) + c == a + (b + c)
                it "should satisfy additive commutativity" $ do
                    property $ \(a :: a) b -> a + b == b + a
                it "should satisfy additive identity" $ do
                    property $ \(a :: a) -> a + zero == a
                it "should satisfy additive inverse" $ do
                    property $ \(a :: a) -> a + negate a == zero

specGroup :: Spec
specGroup = do
    specGroup' @BN254_G1_Point
    specGroup' @BN254_G2_Point

    specGroup' @BLS12_381_G1_Point
    specGroup' @BLS12_381_G2_Point

    specGroup' @Pallas_Point
    specGroup' @Vesta_Point

    specGroup' @Secp256k1_Point

    specGroup' @Ed25519_Point

    specGroup' @Pluto_Point
    specGroup' @Eris_Point
