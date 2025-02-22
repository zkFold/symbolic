module Examples.RSA (exampleRSA) where

import           ZkFold.Symbolic.Algorithms.RSA  (PrivateKey, PublicKey, RSA, sign, verify)
import           ZkFold.Symbolic.Cardano.Types   (Bool)
import           ZkFold.Symbolic.Data.ByteString (ByteString)

exampleRSA :: RSA 2048 256 c => ByteString 256 c -> PrivateKey 2048 c -> PublicKey 2048 c -> Bool c
exampleRSA msg priv pub = verify msg (sign msg priv) pub
