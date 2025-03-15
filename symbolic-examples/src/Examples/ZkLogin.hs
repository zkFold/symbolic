module Examples.ZkLogin (exampleZkLoginNoSig) where

import qualified ZkFold.Symbolic.Algorithms.RSA            as RSA
import           ZkFold.Symbolic.Cardano.Contracts.ZkLogin (PublicInput, zkLoginNoSig)
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.JWT
import           ZkFold.Symbolic.Data.JWT.Google
import           ZkFold.Symbolic.Data.JWT.RS256

exampleZkLoginNoSig
    :: forall ctx
    .  RSA.RSA 2048 10328 ctx
    => TokenBits (GooglePayload ctx)
    => TokenHeader ctx
    -> GooglePayload ctx
    -> Signature "RS256" ctx
    -> ByteString 64 ctx
    -> ByteString 256 ctx
    -> Certificate ctx
    -> PublicInput ctx
    -> Bool ctx
exampleZkLoginNoSig = zkLoginNoSig
