module Examples.JWT (exampleJWTSerialisation) where

import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.JWT
import           ZkFold.Symbolic.Data.JWT.Google
import           ZkFold.Symbolic.Data.VarByteString (VarByteString)

exampleJWTSerialisation
    :: Symbolic c
    => TokenBits (GooglePayload c)
    => TokenHeader c -> GooglePayload c -> VarByteString 10328 c
exampleJWTSerialisation = tokenBits
