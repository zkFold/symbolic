{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module ZkFold.Symbolic.Examples.UInt (
  exampleUIntMul,
  exampleUIntProductMod,
  exampleUIntDivMod,
  exampleUIntExpMod,
  exampleUIntStrictAdd,
  exampleUIntStrictMul,
  exampleUIntResize,
  exampleUIntLeq,
) where

import Data.Type.Equality (type (~))
import GHC.Generics ((:*:) (..))
import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (KnownNat, type (*))
import ZkFold.Symbolic.Compat (CompatData)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.Combinators (
  Ceil,
  GetRegisterSize,
  KnownRegisterSize,
  KnownRegisters,
  NumberOfRegisters,
  resize,
 )
import ZkFold.Symbolic.Data.Ord ((<=))
import ZkFold.Symbolic.Data.UInt (OrdWord, StrictNum (..), UInt, expMod, productMod)
import ZkFold.Symbolic.V2 (Symbolic)

exampleUIntMul
  :: (KnownNat n, KnownRegisterSize r, Symbolic c)
  => UInt n r c -> UInt n r c -> UInt n r c
exampleUIntMul = (*)

exampleUIntProductMod
  :: KnownNat n
  => KnownRegisterSize r
  => KnownNat (Ceil (GetRegisterSize c n r) OrdWord)
  => KnownRegisters c n r
  => Symbolic c
  => UInt n r c
  -> UInt n r c
  -> UInt n r c
  -> (UInt n r :*: UInt n r) c
exampleUIntProductMod x y z = let (p, m) = productMod x y z in p :*: m

exampleUIntDivMod
  :: ( KnownNat n
     , KnownRegisterSize r
     , Symbolic c
     , NumberOfRegisters c n r ~ k
     , KnownNat k
     , KnownNat (Ceil (GetRegisterSize c n r) OrdWord)
     )
  => UInt n r c
  -> UInt n r c
  -> (UInt n r :*: UInt n r) c
exampleUIntDivMod x y = let (d, m) = divMod x y in d :*: m

exampleUIntExpMod
  :: forall n p m r c
   . Symbolic c
  => KnownRegisterSize r
  => KnownNat p
  => KnownNat n
  => KnownNat m
  => KnownNat (2 * m)
  => KnownRegisters c (2 * m) r
  => KnownNat (Ceil (GetRegisterSize c (2 * m) r) OrdWord)
  => UInt n r c
  -> UInt p r c
  -> UInt m r c
  -> UInt m r c
exampleUIntExpMod = expMod @_ @n @p @m @r

exampleUIntStrictAdd
  :: (KnownNat n, KnownRegisterSize r, Symbolic c)
  => UInt n r c -> UInt n r c -> UInt n r c
exampleUIntStrictAdd = strictAdd

exampleUIntStrictMul
  :: (KnownNat n, KnownRegisterSize r, Symbolic c)
  => UInt n r c -> UInt n r c -> UInt n r c
exampleUIntStrictMul = strictMul

exampleUIntResize
  :: (KnownNat n, KnownNat k, KnownRegisterSize r, Symbolic c)
  => UInt n r c -> UInt k r c
exampleUIntResize = resize

exampleUIntLeq
  :: ( KnownNat n
     , KnownRegisterSize r
     , Symbolic c
     , KnownRegisters c n r
     , regSize ~ GetRegisterSize c n r
     , KnownNat (Ceil regSize OrdWord)
     )
  => UInt n r c
  -> UInt n r c
  -> CompatData Bool c
exampleUIntLeq = (<=)
