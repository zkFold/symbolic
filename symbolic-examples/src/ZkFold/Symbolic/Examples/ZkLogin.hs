module ZkFold.Symbolic.Examples.ZkLogin where

import ZkFold.Data.Eq
import ZkFold.Symbolic.Algorithm.Hash.SHA2
import qualified ZkFold.Symbolic.Algorithm.RSA as RSA
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Data.JWT
import ZkFold.Symbolic.Data.JWT.Google
import ZkFold.Symbolic.Data.JWT.RS256
import ZkFold.Symbolic.Data.VarByteString
import Prelude (($))

type PublicInput ctx = ByteString 256 ctx

zkLogin
  :: forall ctx
   . RSA.RSA 2048 10328 ctx
  => TokenBits GooglePayload
  => TokenHeader ctx
  -> GooglePayload ctx
  -> Signature "RS256" ctx
  -> ByteString 64 ctx
  -> ByteString 256 ctx
  -> Certificate ctx
  -> PublicInput ctx
  -> Bool ctx
zkLogin jHeader jPayload jSignature amount recipient certificate pi = tokenValid && piValid
 where
  (tokenValid, tokenHash) = verifyJWT @"RS256" jHeader jPayload jSignature certificate
  truePi = sha2Var @"SHA256" $ plEmail jPayload @+ fromByteString tokenHash @+ fromByteString amount @+ fromByteString recipient
  piValid = truePi == pi

zkLoginNoSig
  :: forall ctx
   . RSA.RSA 2048 10328 ctx
  => TokenBits GooglePayload
  => TokenHeader ctx
  -> GooglePayload ctx
  -> Signature "RS256" ctx
  -> ByteString 64 ctx
  -> ByteString 256 ctx
  -> Certificate ctx
  -> PublicInput ctx
  -> Bool ctx
zkLoginNoSig jHeader jPayload _ amount recipient _ pi = piValid
 where
  tokenHash = sha2Var @"SHA256" $ tokenBits jHeader jPayload
  truePi = sha2Var @"SHA256" $ plEmail jPayload @+ fromByteString tokenHash @+ fromByteString amount @+ fromByteString recipient
  piValid = truePi == pi

exampleZkLoginNoSig
  :: forall ctx
   . RSA.RSA 2048 10328 ctx
  => TokenBits GooglePayload ctx
  => TokenHeader ctx
  -> GooglePayload ctx
  -> Signature "RS256" ctx
  -> ByteString 64 ctx
  -> ByteString 256 ctx
  -> Certificate ctx
  -> PublicInput ctx
  -> Bool ctx
exampleZkLoginNoSig = zkLoginNoSig
