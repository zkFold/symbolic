{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Cardano.Contracts.ZkLogin
    ( PublicInput
    , ZkLoginInput (..)
    , zkLogin
    , zkLoginNoSig
    , zkLoginMock
    ) where

import           GHC.Generics                         (Generic)
import           Prelude                              (($))

import           ZkFold.Symbolic.Algorithms.Hash.SHA2
import           ZkFold.Symbolic.Algorithms.RSA
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.Input
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

data ZkLoginInput ctx =
    ZkLoginInput
        { zkSecret      :: ClientSecret ctx
        , zkAmount      :: ByteString 64 ctx
        , zkRecipient   :: ByteString 256 ctx
        , zkCertificate :: Certificate ctx
        , zkPI          :: PublicInput ctx
        }
        deriving Generic

deriving instance
    ( Symbolic ctx
    , KnownRegisters ctx PubExponentSize 'Auto
    , KnownRegisters ctx 2048 'Auto
    ) => SymbolicData (ZkLoginInput ctx)
instance
    ( Symbolic ctx
    , KnownRegisters ctx PubExponentSize 'Auto
    , KnownRegisters ctx 2048 'Auto
    ) => SymbolicInput (ZkLoginInput ctx) where
    isValid ZkLoginInput{..} = isValid zkAmount

zkLoginMock
    :: forall ctx
    .  RSA 2048 10328 ctx
    => SecretBits ctx
    => ZkLoginInput ctx
    -> Bool ctx
zkLoginMock _ = true
