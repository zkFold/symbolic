module Tests.Symbolic.Algorithm.JWT (specJWT) where

import Codec.Crypto.RSA (generateKeyPair)
import qualified Codec.Crypto.RSA as R
import Data.Function (($))
import System.Random (mkStdGen)
import Test.Hspec (Spec, describe)
import Test.QuickCheck (arbitrary, withMaxSuccess, (.&.), (===))
import Prelude (pure)
import qualified Prelude as P

import Tests.Common (it, toss)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (Fr)
import ZkFold.Algebra.Number
import ZkFold.Symbolic.Algorithm.RSA
import ZkFold.Symbolic.Data.Bool (Bool (fromBool))
import ZkFold.Symbolic.Data.JWT
import ZkFold.Symbolic.Data.JWT.Google
import ZkFold.Symbolic.Data.JWT.RS256
import ZkFold.Symbolic.Data.JWT.Twitch
import ZkFold.Symbolic.Data.VarByteString (VarByteString, fromNatural)

specJWT :: Spec
specJWT = do
  describe "JWT sign and verify" $ do
    it "signs and verifies correctly" $ withMaxSuccess 10 $ do
      x <- toss $ (2 :: Natural) ^ (32 :: Natural)
      kidBits <- toss $ (2 :: Natural) ^ (320 :: Natural)

      let gen = mkStdGen (P.fromIntegral x)
          (R.PublicKey {..}, R.PrivateKey {..}, _) = generateKeyPair gen 2048
          prvkey = PrivateKey (fromConstant private_d) (fromConstant private_n)
          pubkey = PublicKey (fromConstant public_e) (fromConstant public_n)
          kid = fromNatural 320 kidBits :: VarByteString 320 Fr
          skey = SigningKey kid prvkey
          cert = Certificate kid pubkey

      (payloadG :: GooglePayload Fr) <- arbitrary
      (payloadT :: TwitchPayload Fr) <- arbitrary

      let (headerG, sigG) = signPayload @"RS256" payloadG skey
          (checkG, _) = verifyJWT @"RS256" headerG payloadG sigG cert

      let (headerT, sigT) = signPayload @"RS256" payloadT skey
          (checkT, _) = verifyJWT @"RS256" headerT payloadT sigT cert

      pure $ fromBool checkG === one .&. fromBool checkT === one
