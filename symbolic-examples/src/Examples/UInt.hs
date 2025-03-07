{-# LANGUAGE TypeOperators #-}

module Examples.UInt (
    exampleUIntMul,
    exampleUIntProductMod,
    exampleUIntDivMod,
    exampleUIntExpMod,
    exampleUIntStrictAdd,
    exampleUIntStrictMul,
    exampleUIntResize,
    exampleUIntLeq,
  ) where

import           Control.DeepSeq                  (NFData)
import           Data.Type.Equality               (type (~))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number (KnownNat, type (*))
import           ZkFold.Base.Data.Vector          (Vector)
import           ZkFold.Symbolic.Class            (Symbolic (BaseField))
import           ZkFold.Symbolic.Data.Combinators (Ceil, GetRegisterSize, KnownRegisterSize, KnownRegisters,
                                                   NumberOfRegisters, resize)
import           ZkFold.Symbolic.Data.UInt        (OrdWord, StrictNum (..), UInt, expMod, productMod)
import           ZkFold.Symbolic.Data.Bool         (Bool)
import           ZkFold.Symbolic.Data.Ord          ((<=))


exampleUIntMul ::
  (KnownNat n, KnownRegisterSize r, Symbolic c) =>
  UInt n r c -> UInt n r c -> UInt n r c
exampleUIntMul = (*)

exampleUIntProductMod
    :: KnownNat n
    => KnownRegisterSize r
    => KnownNat (Ceil (GetRegisterSize (BaseField c) n r) OrdWord)
    => KnownNat (NumberOfRegisters (BaseField c) n r)
    => Symbolic c
    => UInt n r c -> UInt n r c -> UInt n r c -> (UInt n r c, UInt n r c)
exampleUIntProductMod = productMod

exampleUIntDivMod ::
  (KnownNat n, KnownRegisterSize r, Symbolic c,
   NumberOfRegisters (BaseField c) n r ~ k, KnownNat k,
   KnownNat (Ceil (GetRegisterSize (BaseField c) n r) OrdWord)) =>
  UInt n r c -> UInt n r c -> (UInt n r c, UInt n r c)
exampleUIntDivMod = divMod

exampleUIntExpMod
  :: forall n p m r c
  .  Symbolic c
  => KnownRegisterSize r
  => KnownNat p
  => KnownNat n
  => KnownNat m
  => KnownNat (2 * m)
  => KnownRegisters c (2 * m) r
  => KnownNat (Ceil (GetRegisterSize (BaseField c) (2 * m) r) OrdWord)
  => NFData (c (Vector (NumberOfRegisters (BaseField c) (2 * m) r)))
  => UInt n r c
  -> UInt p r c
  -> UInt m r c
  -> UInt m r c
exampleUIntExpMod = expMod @c @n @p @m @r

exampleUIntStrictAdd ::
  (KnownNat n, KnownRegisterSize r, Symbolic c) =>
  UInt n r c -> UInt n r c -> UInt n r c
exampleUIntStrictAdd = strictAdd

exampleUIntStrictMul ::
  (KnownNat n, KnownRegisterSize r, Symbolic c) =>
  UInt n r c -> UInt n r c -> UInt n r c
exampleUIntStrictMul = strictMul

exampleUIntResize ::
  (KnownNat n, KnownNat k, KnownRegisterSize r, Symbolic c) =>
  UInt n r c -> UInt k r c
exampleUIntResize = resize

exampleUIntLeq ::
  ( KnownNat n, KnownRegisterSize r, Symbolic c
  , KnownRegisters c n r
  , regSize ~ GetRegisterSize (BaseField c) n r
  , KnownNat (Ceil regSize OrdWord)
  ) => UInt n r c -> UInt n r c -> Bool c
exampleUIntLeq = (<=)
