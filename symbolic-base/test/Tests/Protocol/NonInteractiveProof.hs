{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Protocol.NonInteractiveProof (specNonInteractiveProof) where

import           Data.ByteString                        (ByteString)
import           Data.Typeable                          (Proxy (..), Typeable, typeRep)
import           Prelude                                hiding (Fractional (..), Num (..), length)
import           Test.Hspec                             (Spec, describe, it)
import           Test.QuickCheck                        (Arbitrary (..), Testable (property),
                                                         withMaxSuccess)

import           ZkFold.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Algebra.EllipticCurve.Class     (ScalarFieldOf)
import           ZkFold.Algebra.Polynomial.Univariate   (PolyVec)
import           ZkFold.Data.Vector                     (Vector)
import           ZkFold.Protocol.KZG                    (KZG)
import           ZkFold.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Protocol.Plonkup                (Plonkup)

propNonInteractiveProof :: forall a . (NonInteractiveProof a) => (a, Witness a) -> Bool
propNonInteractiveProof (a, w) =
    let sp = setupProve @a a
        sv = setupVerify @a a
        (i, p) = prove @a sp w
    in verify @a sv i p

specNonInteractiveProof' :: forall a . (Typeable a, NonInteractiveProof a,
    Show a, Show (Witness a), Arbitrary a, Arbitrary (Witness a)) => Spec
specNonInteractiveProof' = do
    describe "Non-interactive proof protocol specification" $ do
        describe ("Type: " ++ show (typeRep (Proxy :: Proxy a))) $ do
            describe "All correct proofs" $ do
                it "should validate" $ withMaxSuccess 10 $ property $ propNonInteractiveProof @a

specNonInteractiveProof :: Spec
specNonInteractiveProof = do
    specNonInteractiveProof' @(KZG BLS12_381_G1_JacobianPoint BLS12_381_G2_JacobianPoint 32 (PolyVec (ScalarFieldOf BLS12_381_G1_JacobianPoint)))
    specNonInteractiveProof' @(Plonkup (Vector 1) (Vector 2) 32 BLS12_381_G1_JacobianPoint BLS12_381_G2_JacobianPoint ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_JacobianPoint)))
