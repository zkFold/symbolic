{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Examples.FFA (
  exampleFFAaddNative,
  exampleFFAmulNative,
  exampleFFAinvNative,
  exampleFFAaddForeign,
  exampleFFAmulForeign,
  exampleFFAinvForeign,
) where

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Pasta
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Fixed))
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA)

type Prime256_1 = FpModulus

type Prime256_2 = FqModulus

type RegSize = Fixed 16

type FFA1 = FFA Prime256_1 RegSize

type FFA2 = FFA Prime256_2 RegSize

type KnownFFA1 c = KnownFFA Prime256_1 RegSize c

type KnownFFA2 c = KnownFFA Prime256_2 RegSize c

exampleFFAaddNative :: (Symbolic c, KnownFFA1 c) => FFA1 c -> FFA1 c -> FFA1 c
exampleFFAaddNative = (+)

exampleFFAmulNative :: (Symbolic c, KnownFFA1 c) => FFA1 c -> FFA1 c -> FFA1 c
exampleFFAmulNative = (*)

exampleFFAinvNative :: (Symbolic c, KnownFFA1 c) => FFA1 c -> FFA1 c
exampleFFAinvNative = finv

exampleFFAaddForeign :: (Symbolic c, KnownFFA2 c) => FFA2 c -> FFA2 c -> FFA2 c
exampleFFAaddForeign = (+)

exampleFFAmulForeign :: (Symbolic c, KnownFFA2 c) => FFA2 c -> FFA2 c -> FFA2 c
exampleFFAmulForeign = (*)

exampleFFAinvForeign :: (Symbolic c, KnownFFA2 c) => FFA2 c -> FFA2 c
exampleFFAinvForeign = finv
