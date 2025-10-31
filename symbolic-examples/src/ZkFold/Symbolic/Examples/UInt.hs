{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}

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
import ZkFold.Symbolic.Compat (CompatData (CompatData, compatData))
import Data.Function ((.))

exampleUIntMul
  :: (KnownNat n, KnownRegisterSize r, Symbolic c)
  => CompatData (UInt n r) c
  -> CompatData (UInt n r) c
  -> CompatData (UInt n r) c
exampleUIntMul = (*)

exampleUIntProductMod
  :: KnownNat n
  => KnownRegisterSize r
  => KnownNat (Ceil (GetRegisterSize c n r) OrdWord)
  => KnownRegisters c n r
  => Symbolic c
  => CompatData (UInt n r) c
  -> CompatData (UInt n r) c
  -> CompatData (UInt n r) c
  -> (CompatData (UInt n r) :*: CompatData (UInt n r)) c
exampleUIntProductMod (CompatData x) (CompatData y) (CompatData z) =
  let (p, m) = productMod x y z in CompatData p :*: CompatData m

exampleUIntDivMod
  :: ( KnownNat n
     , KnownRegisterSize r
     , Symbolic c
     , NumberOfRegisters c n r ~ k
     , KnownNat k
     , KnownNat (Ceil (GetRegisterSize c n r) OrdWord)
     )
  => CompatData (UInt n r) c
  -> CompatData (UInt n r) c
  -> (CompatData (UInt n r) :*: CompatData (UInt n r)) c
exampleUIntDivMod (CompatData x) (CompatData y) =
  let (d, m) = divMod x y in CompatData d :*: CompatData m

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
  => CompatData (UInt n r) c
  -> CompatData (UInt p r) c
  -> CompatData (UInt m r) c
  -> CompatData (UInt m r) c
exampleUIntExpMod (CompatData x) (CompatData y) (CompatData m) =
  CompatData (expMod @_ @n @p @m @r x y m)

exampleUIntStrictAdd
  :: (KnownNat n, KnownRegisterSize r, Symbolic c)
  => CompatData (UInt n r) c
  -> CompatData (UInt n r) c
  -> CompatData (UInt n r) c
exampleUIntStrictAdd (CompatData x) (CompatData y) = CompatData (strictAdd x y)

exampleUIntStrictMul
  :: (KnownNat n, KnownRegisterSize r, Symbolic c)
  => CompatData (UInt n r) c
  -> CompatData (UInt n r) c
  -> CompatData (UInt n r) c
exampleUIntStrictMul (CompatData x) (CompatData y) = CompatData (strictMul x y)

exampleUIntResize
  :: (KnownNat n, KnownNat k, KnownRegisterSize r, Symbolic c)
  => CompatData (UInt n r) c
  -> CompatData (UInt k r) c
exampleUIntResize = CompatData . resize . compatData

exampleUIntLeq
  :: ( KnownNat n
     , KnownRegisterSize r
     , Symbolic c
     , KnownRegisters c n r
     , regSize ~ GetRegisterSize c n r
     , KnownNat (Ceil regSize OrdWord)
     )
  => CompatData (UInt n r) c
  -> CompatData (UInt n r) c
  -> CompatData Bool c
exampleUIntLeq = (<=)
