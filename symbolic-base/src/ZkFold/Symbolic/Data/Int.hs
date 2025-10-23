{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Int where

import qualified Data.Bool as Haskell
import Data.Semigroup ((<>))
import GHC.Generics (Generic, type (:*:) (..))
import Test.QuickCheck (Arbitrary)
import Text.Show (Show)
import Prelude (Integer, ($), (.))
import qualified Prelude as Haskell hiding ((-))

import ZkFold.Algebra.Class hiding (Euclidean (..))
import ZkFold.Algebra.Number
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq
import qualified ZkFold.Data.Vector as V
import ZkFold.Symbolic.Class (Arithmetic, Symbolic)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Ord
import ZkFold.Symbolic.Data.Register (bitsOfR)
import ZkFold.Symbolic.Data.UInt
import ZkFold.Symbolic.Data.UIntData (UIntData (MkUIntData))

newtype Int n c = Int {intToUInt :: UInt n c}
  deriving (Eq, Generic, Show, SymbolicData, SymbolicInput)

uintToInt :: UInt n c -> Int n c
uintToInt = Int

deriving newtype instance
  (Haskell.Monoid m, Collect m (UInt n c)) => Collect m (Int n c)

deriving newtype instance (Symbolic c, KnownUInt n c) => Arbitrary (Int n c)

deriving newtype instance
  (Symbolic c, KnownUInt n c) => FromConstant Natural (Int n c)

deriving newtype instance
  (Symbolic c, KnownUInt n c) => FromConstant Integer (Int n c)

deriving newtype instance
  (Symbolic c, KnownUInt n c) => Scale Natural (Int n c)

deriving newtype instance
  (Symbolic c, KnownUInt n c) => Scale Integer (Int n c)

deriving newtype instance (Symbolic c, KnownUInt n c) => Zero (Int n c)

deriving newtype instance
  (Symbolic c, KnownUInt n c) => AdditiveSemigroup (Int n c)

instance (Symbolic c, KnownUInt n c) => AdditiveMonoid (Int n c)

deriving newtype instance
  (Symbolic c, KnownUInt n c) => AdditiveGroup (Int n c)

deriving newtype instance
  (Symbolic c, KnownUInt n c) => MultiplicativeSemigroup (Int n c)

deriving newtype instance
  (Symbolic c, KnownUInt n c) => MultiplicativeMonoid (Int n c)

instance (Symbolic c, KnownUInt n c) => Semiring (Int n c)

instance (Symbolic c, KnownUInt n c) => Ring (Int n c)

instance (Symbolic c, KnownUInt n c) => Exponent (Int n c) Natural where
  (^) = natPow

------------------------------------------------------------------------------

instance (Arithmetic a, KnownUInt n a) => ToConstant (Int n a) where
  type Const (Int n a) = Integer
  toConstant i@(Int u) =
    Haskell.bool
      (Haskell.toInteger . toConstant $ toConstant u)
      (negate . Haskell.toInteger . toConstant . toConstant $ negate u)
      (fromBool (isNegative i) Haskell.== one)

-- FIXME: what if head register is 0-sized?
isNegative :: forall n c. (Symbolic c, KnownUInt n c) => Int n c -> Bool c
isNegative (Int (UInt (MkUIntData hi _))) = V.head (bitsOfR hi)

isNotNegative :: forall n c. (Symbolic c, KnownUInt n c) => Int n c -> Bool c
isNotNegative i = not (isNegative i)

abs :: forall c n. (Symbolic c, KnownUInt n c) => Int n c -> Int n c
abs i = bool i (negate i) (isNegative i)

instance (Symbolic c, KnownUInt n c) => SemiEuclidean (Int n c) where
  divMod i1 i2 = (r1, r2)
   where
    r1 :*: r2 = ifThenElse (i1 == zero) dm_pp ite_nn

    (Int u1, Int u2) = (abs i1, abs i2)
    (d, m) = divMod u1 u2

    ite_nn = ifThenElse (isNegative i1 && isNegative i2) dm_mm ite_np
    ite_np = ifThenElse (isNegative i1 && isNotNegative i2) dm_mp ite_pm
    ite_pm = ifThenElse (isNotNegative i1 && isNegative i2) dm_pm dm_pp

    dm_mm = Int d :*: negate (Int m)
    dm_mp = bool ((negate (Int d) - one) :*: (i2 - Int m)) (negate (Int d) :*: Int m) (m == zero)
    dm_pm = bool ((negate (Int d) - one) :*: (i2 + Int m)) (negate (Int d) :*: Int m) (m == zero)
    dm_pp = Int d :*: Int m

quotRem
  :: forall n c
   . (Symbolic c, KnownUInt n c)
  => Int n c -> Int n c -> (Int n c, Int n c)
quotRem i1 i2 = (r1, r2)
 where
  r1 :*: r2 = ifThenElse (isNegative i1 && isNegative i2) dm_mm ite_tf

  (Int u1, Int u2) = (abs i1, abs i2)
  (d, m) = divMod u1 u2

  ite_tf = ifThenElse (isNegative i1 && isNotNegative i2) dm_mp ite_pm
  ite_pm = ifThenElse (isNotNegative i1 && isNegative i2) dm_pm dm_pp

  dm_mm = Int d :*: negate (Int m)
  dm_mp = negate (Int d) :*: negate (Int m)
  dm_pm = negate (Int d) :*: Int m
  dm_pp = Int d :*: Int m

quot :: forall n c. (Symbolic c, KnownUInt n c) => Int n c -> Int n c -> Int n c
quot i1 i2 = Haskell.fst $ quotRem i1 i2

rem :: forall n c. (Symbolic c, KnownUInt n c) => Int n c -> Int n c -> Int n c
rem i1 i2 = Haskell.snd $ quotRem i1 i2

instance (Symbolic c, KnownUInt n c) => Ord (Int n c) where
  type OrderingOf (Int n c) = Ordering c
  compare x y =
    compare (isNegative y) (isNegative x)
      <> compare (intToUInt x) (intToUInt y)
