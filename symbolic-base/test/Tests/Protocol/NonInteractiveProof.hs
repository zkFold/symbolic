{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Protocol.NonInteractiveProof (specNonInteractiveProof) where

import           Data.ByteString                             (ByteString)
import           Data.Typeable                               (Proxy (..), Typeable, typeRep)
import           GHC.Generics                                (U1 (..))
import           Prelude                                     hiding (Fractional (..), Num (..), length)
import           Test.Hspec                                  (Spec, describe, it)
import           Test.QuickCheck                             (Arbitrary (..), Arbitrary1 (..), Testable (property),
                                                              withMaxSuccess)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Algebra.EllipticCurve.Class     (ScalarFieldOf)
import           ZkFold.Base.Algebra.Polynomials.Univariate  (PolyVec)
import           ZkFold.Base.Data.Vector                     (Vector)
import           ZkFold.Base.Protocol.KZG                    (KZG)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonk                  (Plonk)
import           ZkFold.Base.Protocol.Plonkup                (Plonkup)

propNonInteractiveProof :: forall a . (NonInteractiveProof a) => (a, Witness a) -> Bool
propNonInteractiveProof (a, w) =
    let sp = setupProve @a a
        sv = setupVerify @a a
        (i, p) = prove @a sp w
    in verify @a sv i p

specNonInteractiveProof' :: forall a . (Typeable a, NonInteractiveProof a,
    Show a, Show (Witness a), Arbitrary a, Arbitrary (Witness a)) => Spec
specNonInteractiveProof' = do
    describe "Non-interactive proof protocol specification (SLOW)" $ do
        describe ("Type: " ++ show (typeRep (Proxy :: Proxy a))) $ do
            describe "All correct proofs" $ do
                it "should validate" $ withMaxSuccess 10 $ property $ propNonInteractiveProof @a

instance Arbitrary (U1 a) where
  arbitrary = return U1
instance Arbitrary1 U1 where
  liftArbitrary _ = return U1

specNonInteractiveProof :: Spec
specNonInteractiveProof = do
    specNonInteractiveProof' @(KZG BLS12_381_G1_Point BLS12_381_G2_Point 32 (PolyVec (ScalarFieldOf BLS12_381_G1_Point)))
    specNonInteractiveProof' @(Plonk (Vector 1) 32 (Vector 2) BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)))
    specNonInteractiveProof' @(Plonkup (Vector 1) 32 (Vector 2) BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)))
