{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module ZkFold.Symbolic.Examples.UInt (
  exampleUIntMul,
  exampleUIntDivMod,
  exampleUIntExpMod,
  exampleUIntStrictAdd,
  exampleUIntStrictMul,
  exampleUIntResize,
  exampleUIntLeq,
) where

import GHC.Generics ((:*:) (..))
import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (type (*))
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.Ord ((<=))
import ZkFold.Symbolic.Data.UInt

exampleUIntMul
  :: (KnownUInt n c, Symbolic c) => UInt n c -> UInt n c -> UInt n c
exampleUIntMul = (*)

exampleUIntDivMod
  :: (KnownUInt n c, Symbolic c)
  => UInt n c
  -> UInt n c
  -> (UInt n :*: UInt n) c
exampleUIntDivMod x y = let (d, m) = divMod x y in d :*: m

exampleUIntExpMod
  :: forall n p m c
   . (Symbolic c, KnownUInt n c, KnownUInt p c, KnownUInt m c)
  => KnownUInt (2 * m) c
  => UInt n c
  -> UInt p c
  -> UInt m c
  -> UInt m c
exampleUIntExpMod = expMod

exampleUIntStrictAdd
  :: (KnownUInt n c, Symbolic c) => UInt n c -> UInt n c -> UInt n c
exampleUIntStrictAdd = (+!)

exampleUIntStrictMul
  :: (KnownUInt n c, Symbolic c) => UInt n c -> UInt n c -> UInt n c
exampleUIntStrictMul = (*!)

exampleUIntResize
  :: forall n k c
   . (KnownUInt n c, KnownUInt k c, Symbolic c)
  => UInt n c -> UInt k c
exampleUIntResize = resizeUInt

exampleUIntLeq :: (KnownUInt n c, Symbolic c) => UInt n c -> UInt n c -> Bool c
exampleUIntLeq = (<=)
