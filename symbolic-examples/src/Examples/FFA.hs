{-# OPTIONS_GHC -Wno-orphans #-}

module Examples.FFA
  ( exampleFFAadd337
  , exampleFFAadd097
  , exampleFFAmul337
  , exampleFFAmul097
  , exampleFFAinv337
  , exampleFFAinv097) where

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.EllipticCurve.Pasta
import           ZkFold.Symbolic.Class                   (Symbolic)
import           ZkFold.Symbolic.Data.Combinators        (RegisterSize (Fixed))
import           ZkFold.Symbolic.Data.FFA                (FFA, KnownFFA)

type Prime256_1 = FpModulus
type Prime256_2 = FqModulus

type RegSize = Fixed 16

type FFA1 = FFA Prime256_1 RegSize
type FFA2 = FFA Prime256_2 RegSize

type KnownFFA1 c = KnownFFA Prime256_1 RegSize c
type KnownFFA2 c = KnownFFA Prime256_2 RegSize c

exampleFFAadd337 :: (Symbolic c, KnownFFA1 c) => FFA1 c -> FFA1 c -> FFA1 c
exampleFFAadd337 = (+)

exampleFFAadd097 :: (Symbolic c, KnownFFA2 c) => FFA2 c -> FFA2 c -> FFA2 c
exampleFFAadd097 = (+)

exampleFFAmul337 :: (Symbolic c, KnownFFA1 c) => FFA1 c -> FFA1 c -> FFA1 c
exampleFFAmul337 = (*)

exampleFFAmul097 :: (Symbolic c, KnownFFA2 c) => FFA2 c -> FFA2 c -> FFA2 c
exampleFFAmul097 = (*)

exampleFFAinv337 :: (Symbolic c, KnownFFA1 c) => FFA1 c -> FFA1 c
exampleFFAinv337 = finv

exampleFFAinv097 :: (Symbolic c, KnownFFA2 c) => FFA2 c -> FFA2 c
exampleFFAinv097 = finv
