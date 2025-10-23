module ZkFold.Symbolic.Examples.JWT (exampleJWTSerialisation) where

import ZkFold.Symbolic.Data.JWT
import ZkFold.Symbolic.Data.JWT.Google
import ZkFold.Symbolic.Data.VarByteString (VarByteString)
import ZkFold.Symbolic.Class (Symbolic)

exampleJWTSerialisation
  :: Symbolic c => TokenHeader c -> GooglePayload c -> VarByteString 10328 c
exampleJWTSerialisation = tokenBits
