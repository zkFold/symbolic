module ZkFold.Symbolic.Cardano.Contracts.ZkLogin
    ( PublicInput
    , zkLogin
    , zkLoginNoSig
    ) where

import           Prelude                              (($))

import           ZkFold.Symbolic.Algorithms.Hash.SHA2
import           ZkFold.Symbolic.Algorithms.RSA
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.JWT             (Certificate, ClientSecret (..), SecretBits, TokenPayload (..),
                                                       secretBits, verifySignature)
import           ZkFold.Symbolic.Data.VarByteString

type PublicInput ctx = ByteString 256 ctx

zkLogin
    :: forall ctx
    .  RSA 2048 10328 ctx
    => SecretBits ctx
    => ClientSecret ctx
    -> ByteString 64 ctx
    -> ByteString 256 ctx
    -> Certificate ctx
    -> PublicInput ctx
    -> Bool ctx
zkLogin clientSecret@ClientSecret{..} amount recipient certificate pi = tokenValid && piValid
    where
        (tokenValid, tokenHash) = verifySignature certificate clientSecret
        truePi = sha2Var @"SHA256" $ plEmail csPayload @+ fromByteString tokenHash @+ fromByteString amount @+ fromByteString recipient
        piValid = truePi == pi


zkLoginNoSig
    :: forall ctx
    .  RSA 2048 10328 ctx
    => SecretBits ctx
    => ClientSecret ctx
    -> ByteString 64 ctx
    -> ByteString 256 ctx
    -> Certificate ctx
    -> PublicInput ctx
    -> Bool ctx
zkLoginNoSig clientSecret@ClientSecret{..} amount recipient _ pi = piValid
    where
        tokenHash = sha2Var @"SHA256" $ secretBits clientSecret
        truePi = sha2Var @"SHA256" $ plEmail csPayload @+ fromByteString tokenHash @+ fromByteString amount @+ fromByteString recipient
        piValid = truePi == pi
