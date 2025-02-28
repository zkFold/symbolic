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
import           ZkFold.Base.Data.Vector                     (Vector)
import           ZkFold.Base.Protocol.KZG                    (KZG)
import           ZkFold.Base.Protocol.NonInteractiveProof    (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonk                  (Plonk)
import           ZkFold.Base.Protocol.Plonkup                (Plonkup)

propNonInteractiveProof :: forall a core . (NonInteractiveProof a core) => (a, Witness a) -> Bool
propNonInteractiveProof (a, w) =
    let sp = setupProve @a @core a
        sv = setupVerify @a @core a
        (i, p) = prove @a @core sp w
    in verify @a @core sv i p

specNonInteractiveProof' :: forall a core . (Typeable a, NonInteractiveProof a core,
    Show a, Show (Witness a), Arbitrary a, Arbitrary (Witness a)) => Spec
specNonInteractiveProof' = do
    describe "Non-interactive proof protocol specification (SLOW)" $ do
        describe ("Type: " ++ show (typeRep (Proxy :: Proxy a))) $ do
            describe "All correct proofs" $ do
                it "should validate" $ withMaxSuccess 10 $ property $ propNonInteractiveProof @a @core

instance Arbitrary (U1 a) where
  arbitrary = return U1
instance Arbitrary1 U1 where
  liftArbitrary _ = return U1

specNonInteractiveProof :: Spec
specNonInteractiveProof = do
    specNonInteractiveProof' @(KZG BLS12_381_G1_Point BLS12_381_G2_Point 32) @HaskellCore
    specNonInteractiveProof' @(Plonk U1 (Vector 1) 32 (Vector 2) BLS12_381_G1_Point BLS12_381_G2_Point ByteString) @HaskellCore
    specNonInteractiveProof' @(Plonkup U1 (Vector 1) 32 (Vector 2) BLS12_381_G1_Point BLS12_381_G2_Point ByteString) @HaskellCore
