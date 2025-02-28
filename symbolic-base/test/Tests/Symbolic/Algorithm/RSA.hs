{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}

module Tests.Symbolic.Algorithm.RSA (specRSA) where

import           Codec.Crypto.RSA                            (generateKeyPair)
import qualified Codec.Crypto.RSA                            as R
import           Data.Function                               (($))
import           GHC.Generics                                (Par1 (..))
import           Prelude                                     (pure)
import qualified Prelude                                     as P
import           System.Random                               (RandomGen)
import           Test.Hspec                                  (Spec, describe)
import           Test.QuickCheck                             (Gen, withMaxSuccess, (.&.), (===))
import           Tests.Symbolic.ArithmeticCircuit            (it)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Prelude                              (chooseNatural)
import           ZkFold.Symbolic.Algorithms.RSA
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Combinators            (ilog2)
import           ZkFold.Symbolic.Data.VarByteString          (fromNatural)
import           ZkFold.Symbolic.Interpreter                 (Interpreter (Interpreter))

type I = Interpreter Fr

toss :: Natural -> Gen Natural
toss x = chooseNatural (0, x -! 1)

evalBool :: forall a . Bool (Interpreter a) -> a
evalBool (Bool (Interpreter (Par1 v))) = v

specRSA' :: forall keyLength g . (RandomGen g, RSA keyLength 256 I) => g -> Spec
specRSA' gen = do
    describe ("RSA signature: key length of " P.<> P.show (value @keyLength) P.<> " bits") $ do
        it "signs and verifies correctly" $ withMaxSuccess 10 $ do
            msgBits <- toss $ (2 :: Natural) ^ (256 :: Natural)
            let (R.PublicKey{..}, R.PrivateKey{..}, _) = generateKeyPair gen (P.fromIntegral $ value @keyLength)
                prvkey = PrivateKey (fromConstant private_d) (fromConstant private_n)
                pubkey = PublicKey (fromConstant public_e) (fromConstant public_n)
                msg = fromConstant msgBits

                msgVar = fromNatural (ilog2 msgBits) msgBits

                sig = sign @keyLength @256 @I msg prvkey
                check = verify @keyLength @256 @I msg sig pubkey

                sigV = signVar @keyLength @256 @I msgVar prvkey
                (checkV, _) = verifyVar @keyLength @256 @I msgVar sigV pubkey

            pure $ evalBool check === one .&. evalBool checkV === one

specRSA :: RandomGen g => g -> Spec
specRSA gen = do
    specRSA' @512   gen
    specRSA' @2048  gen
