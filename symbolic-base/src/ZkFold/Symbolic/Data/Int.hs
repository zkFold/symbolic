{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module ZkFold.Symbolic.Data.Int where

import Control.DeepSeq
import qualified Data.Bool as Haskell
import GHC.Generics (Generic, type (:*:) (..))
import Test.QuickCheck (Arbitrary (..))
import Prelude (Integer, ($), (.))
import qualified Prelude as Haskell hiding ((-))

import ZkFold.Algebra.Class hiding (Euclidean (..))
import ZkFold.Algebra.Number
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Ord
import ZkFold.Symbolic.Data.UInt
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Class (Arithmetic, Symbolic)
import GHC.Err (error)
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Iso (Iso (..))

newtype Int n r c = Int {uint :: UInt n r c}
  deriving (Generic, NFData, Haskell.Eq, Haskell.Show, SymbolicData, Eq,
            FromConstant Natural, FromConstant Integer, MultiplicativeMonoid,
            Zero, AdditiveMonoid, Arbitrary, Scale Natural, Scale Integer,
            Semiring, AdditiveSemigroup, AdditiveGroup, MultiplicativeSemigroup)

deriving newtype instance
  (Haskell.Monoid m, Collect m (UInt n r c)) => Collect m (Int n r c)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Exponent (Int n r c) Natural where
  (^) = natPow

------------------------------------------------------------------------------

instance (Arithmetic a, KnownRegisterSize r, KnownNat n) => ToConstant (Int n r a) where
  type Const (Int n r a) = Integer
  toConstant i@(Int u) =
    withGetRegisterSize @n @r @a $
      Haskell.bool
        (Haskell.toInteger $ toConstant u)
        (negate . Haskell.toInteger . toConstant $ negate u)
        (isNegative i Haskell.== true)

isNegative ::
  forall n r c. (Symbolic c, KnownNat n, KnownRegisterSize r) =>
  Int n r c -> Bool c
isNegative (Int (UInt _)) = Bool $ error "TODO" -- fromCircuitF u $ \regs -> do
  -- let hd = Haskell.last $ fromVector regs
  -- (_, h) <- splitExpansion (highRegisterSize @c @n @r -! 1) 1 hd
  -- Haskell.return $ Par1 h

isNotNegative ::
  forall n r c. (Symbolic c, KnownNat n, KnownRegisterSize r) =>
  Int n r c -> Bool c
isNotNegative i = not (isNegative i)

abs :: forall c n r. (Symbolic c, KnownNat n, KnownRegisterSize r) => Int n r c -> Int n r c
abs i = withNumberOfRegisters @n @r @c $ bool i (negate i) (isNegative i)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Iso (Int n r c) (UInt n r c) where
  from (Int u) = u

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Iso (UInt n r c) (Int n r c) where
  from = Int

instance
  ( Symbolic c
  , KnownNat n
  , KnownRegisterSize r
  , KnownRegisters c n r
  )
  => SemiEuclidean (Int n r c)
  where
  divMod i1 i2 = (r1, r2)
   where
    r1 :*: r2 = ifThenElse (i1 == zero) dm_pp ite_nn

    (Int u1, Int u2) = (abs i1, abs i2)
    (d, m) =
      withGetRegisterSize @n @r @c $
        withCeilRegSize @(GetRegisterSize c n r) @OrdWord $
          divMod u1 u2

    ite_nn = ifThenElse (isNegative i1 && isNegative i2) dm_mm ite_np
    ite_np = ifThenElse (isNegative i1 && isNotNegative i2) dm_mp ite_pm
    ite_pm = ifThenElse (isNotNegative i1 && isNegative i2) dm_pm dm_pp

    dm_mm = Int d :*: negate (Int m)
    dm_mp = bool ((negate (Int d) - one) :*: (i2 - Int m)) (negate (Int d) :*: Int m) (m == zero)
    dm_pm = bool ((negate (Int d) - one) :*: (i2 + Int m)) (negate (Int d) :*: Int m) (m == zero)
    dm_pp = Int d :*: Int m

div
  :: forall n r c
   . ( Symbolic c
     , KnownNat n
     , KnownRegisterSize r
     , KnownRegisters c n r
     )
  => Int n r c -> Int n r c -> Int n r c
div i1 i2 = Haskell.fst $ divMod i1 i2

mod
  :: forall n r c
   . ( Symbolic c
     , KnownNat n
     , KnownRegisterSize r
     , KnownRegisters c n r
     )
  => Int n r c -> Int n r c -> Int n r c
mod i1 i2 = Haskell.snd $ divMod i1 i2

quotRem
  :: forall n r c
   . ( Symbolic c
     , KnownNat n
     , KnownRegisterSize r
     , KnownRegisters c n r
     )
  => Int n r c -> Int n r c -> (Int n r c, Int n r c)
quotRem i1 i2 = (r1, r2)
 where
  r1 :*: r2 = ifThenElse (isNegative i1 && isNegative i2) dm_mm ite_tf

  (Int u1, Int u2) = (abs i1, abs i2)
  (d, m) =
    withGetRegisterSize @n @r @c $
      withCeilRegSize @(GetRegisterSize c n r) @OrdWord $
        divMod u1 u2

  ite_tf = ifThenElse (isNegative i1 && isNotNegative i2) dm_mp ite_pm
  ite_pm = ifThenElse (isNotNegative i1 && isNegative i2) dm_pm dm_pp

  dm_mm = Int d :*: negate (Int m)
  dm_mp = negate (Int d) :*: negate (Int m)
  dm_pm = negate (Int d) :*: Int m
  dm_pp = Int d :*: Int m

quot
  :: forall n r c
   . ( Symbolic c
     , KnownNat n
     , KnownRegisterSize r
     , KnownRegisters c n r
     )
  => Int n r c -> Int n r c -> Int n r c
quot i1 i2 = Haskell.fst $ quotRem i1 i2

rem
  :: forall n r c
   . ( Symbolic c
     , KnownNat n
     , KnownRegisterSize r
     , KnownRegisters c n r
     )
  => Int n r c -> Int n r c -> Int n r c
rem i1 i2 = Haskell.snd $ quotRem i1 i2

instance
  ( Symbolic c
  , KnownNat n
  , KnownRegisterSize r
  , KnownRegisters c n r
  )
  => Ord (Int n r c)
  where
  type OrderingOf (Int n r c) = Ordering c

  ordering x y z o = bool (bool x y (o == eq)) z (o == gt)

  compare x y = bool (bool lt eq (x == y)) gt (x > y)

  x <= y = y >= x

  x < y = y > x

  i1 >= i2 = (isNotNegative i1 && isNegative i2) || (ub && not (xor (isNegative i1) (isNegative i2)))
   where
    ub =
      withGetRegisterSize @n @r @c $
        withCeilRegSize @(GetRegisterSize c n r) @OrdWord $
          uint i1 >= uint i2

  i1 > i2 = (isNotNegative i1 && isNegative i2) || (ub && not (xor (isNegative i1) (isNegative i2)))
   where
    ub =
      withGetRegisterSize @n @r @c $
        withCeilRegSize @(GetRegisterSize c n r) @OrdWord $
          uint i1 > uint i2

  max x y = bool x y $ x < y
  min x y = bool x y $ x > y
