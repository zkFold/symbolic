module Examples.ZkLogin (exampleZkLoginNoSig) where

import           ZkFold.Symbolic.Algorithms.RSA
import           ZkFold.Symbolic.Cardano.Contracts.ZkLogin (PublicInput, zkLoginNoSig)
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.JWT

exampleZkLoginNoSig
    :: forall ctx
    .  RSA 2048 10328 ctx
    => SecretBits ctx
    => ClientSecret ctx
    -> ByteString 64 ctx
    -> ByteString 256 ctx
    -> Certificate ctx
    -> PublicInput ctx
    -> Bool ctx
exampleZkLoginNoSig = zkLoginNoSig
