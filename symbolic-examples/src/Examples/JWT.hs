module Examples.JWT (exampleJWTSerialisation) where

import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.JWT
import           ZkFold.Symbolic.Data.VarByteString (VarByteString)

exampleJWTSerialisation
    :: Symbolic c
    => SecretBits c
    => ClientSecret c -> VarByteString 10328 c
exampleJWTSerialisation = secretBits
