{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}

module Tests.Symbolic.Algorithm.JWT (specJWT) where

import           Codec.Crypto.RSA                            (generateKeyPair)
import qualified Codec.Crypto.RSA                            as R
import           Data.Function                               (($))
import           GHC.Generics                                (Par1 (..))
import           Prelude                                     (pure)
import qualified Prelude                                     as P
import           System.Random                               (mkStdGen)
import           Test.Hspec                                  (Spec, describe)
import           Test.QuickCheck                             (Gen, arbitrary, withMaxSuccess, (.&.), (===))
import           Tests.Symbolic.ArithmeticCircuit            (it)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Prelude                              (chooseNatural)
import           ZkFold.Symbolic.Algorithms.RSA
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.JWT
import           ZkFold.Symbolic.Data.JWT.Google
import           ZkFold.Symbolic.Data.JWT.RS256
import           ZkFold.Symbolic.Data.JWT.Twitch
import           ZkFold.Symbolic.Data.VarByteString          (VarByteString, fromNatural)
import           ZkFold.Symbolic.Interpreter                 (Interpreter (Interpreter))

type I = Interpreter Fr

toss :: Natural -> Gen Natural
toss x = chooseNatural (0, x -! 1)

evalBool :: forall a . Bool (Interpreter a) -> a
evalBool (Bool (Interpreter (Par1 v))) = v

specJWT :: Spec
specJWT = do
    describe "JWT sign and verify" $ do
        it "signs and verifies correctly" $ withMaxSuccess 10 $ do
            x <- toss $ (2 :: Natural) ^ (32 :: Natural)
            kidBits <- toss $ (2 :: Natural) ^ (320 :: Natural)

            let gen = mkStdGen (P.fromIntegral x)
                (R.PublicKey{..}, R.PrivateKey{..}, _) = generateKeyPair gen 2048
                prvkey = PrivateKey (fromConstant private_d) (fromConstant private_n)
                pubkey = PublicKey (fromConstant public_e) (fromConstant public_n)
                kid = fromNatural 320 kidBits :: VarByteString 320 I
                skey = SigningKey kid prvkey
                cert = Certificate kid pubkey

            (payloadG :: GooglePayload I) <- arbitrary
            (payloadT :: TwitchPayload I) <- arbitrary

            let (headerG, sigG) = signPayload @"RS256" payloadG skey
                (checkG, _)    = verifyJWT @"RS256" headerG payloadG sigG cert

            let (headerT, sigT) = signPayload @"RS256" payloadT skey
                (checkT, _)    = verifyJWT @"RS256" headerT payloadT sigT cert

            pure $ evalBool checkG === one .&. evalBool checkT === one
