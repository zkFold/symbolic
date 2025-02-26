{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}


module ZkFold.Symbolic.Data.Int where

import           Control.DeepSeq
import qualified Data.Bool                         as Haskell
import           Data.Kind                         (Type)
import           GHC.Generics                      (Generic, Par1 (..))
import           Prelude                           (Integer, return, type (~), ($), (.))
import qualified Prelude                           as Haskell hiding ((-))
import           Test.QuickCheck                   (Arbitrary (..))

import           ZkFold.Base.Algebra.Basic.Class   hiding (Euclidean (..))
import           ZkFold.Base.Algebra.Basic.Number
import qualified ZkFold.Base.Data.Vector           as V
import           ZkFold.Base.Data.Vector           (Vector (..))
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Class        (SymbolicData)
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.UInt
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Interpreter       (Interpreter (..))
import Data.Constraint.Nat
import Data.Constraint (withDict)
import ZkFold.Symbolic.Data.Ord


newtype Int (n :: Natural) (r :: RegisterSize) (c :: (Type -> Type) -> Type) = Int { uint :: UInt (n+1) r c}

deriving instance Generic (Int n r c)
deriving instance (NFData (c (Vector (NumberOfRegisters (BaseField c) (n+1) r)))) => NFData (Int n r c)
deriving instance (Haskell.Eq (c (Vector (NumberOfRegisters (BaseField c) (n+1) r)))) => Haskell.Eq (Int n r c)
deriving instance (Haskell.Show (BaseField c), Haskell.Show (c (Vector (NumberOfRegisters (BaseField c) (n+1) r))))
    => Haskell.Show (Int n r c)
deriving instance (KnownRegisters c (n+1) r, Symbolic c) => SymbolicData (Int n r c)
deriving instance (KnownRegisters c (n+1) r, Symbolic c) => Conditional (Bool c) (Int n r c)
deriving instance (KnownRegisters c (n+1) r, Symbolic c) => Eq (Int n r c)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => FromConstant Natural (Int n r c) where
    fromConstant c = withDict (plusNat @n @1) $ Int (fromConstant c)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => FromConstant Integer (Int n r c) where
    fromConstant c = withDict (plusNat @n @1) $ Int (Haskell.bool (fromConstant c) (negate $ fromConstant (Haskell.negate c)) (c Haskell.<0))

instance MultiplicativeMonoid (Int n r c) => Exponent (Int n r c) Natural where
    (^) = natPow

------------------------------------------------------------------------------

instance (Symbolic (Interpreter a), KnownNat n, KnownRegisterSize r
    , KnownNat (n + 1)
    , regSize ~ GetRegisterSize a (n+1) r
    , KnownNat (Ceil regSize OrdWord)
    , KnownNat (NumberOfRegisters a (n + 1) r)) => ToConstant (Int n r (Interpreter a)) where
    type Const (Int n r (Interpreter a)) = Integer
    toConstant i@(Int u) = withDict (plusNat @n @1) $
        Haskell.bool (Haskell.toInteger $ toConstant u) (negate . Haskell.toInteger . toConstant $ negate u) (isNegative i Haskell.== true)

isNegative :: forall n r c regSize.
    (Symbolic c, KnownNat n, KnownRegisterSize r
    , KnownNat (NumberOfRegisters (BaseField c) (n + 1) r), KnownNat (n + 1)
    , regSize ~ GetRegisterSize (BaseField c) (n+1) r
    , KnownNat (Ceil regSize OrdWord)) => Int n r c -> Bool c
isNegative (Int u) = u >= fromConstant ((2 :: Natural) ^ (value @n))

isNotNegative :: forall n r c regSize.
    ( Symbolic c, KnownNat n, KnownRegisterSize r
    , KnownNat (NumberOfRegisters (BaseField c) (n + 1) r), KnownNat (n + 1)
    , regSize ~ GetRegisterSize (BaseField c) (n+1) r
    , KnownNat (Ceil regSize OrdWord)) => Int n r c -> Bool c
isNotNegative i = not (isNegative i)

abs :: forall c n r regSize.
    (Symbolic c, KnownNat n, KnownRegisterSize r
    , KnownNat (NumberOfRegisters (BaseField c) (n + 1) r), KnownNat (n + 1)
    , regSize ~ GetRegisterSize (BaseField c) (n+1) r
    , KnownNat (Ceil regSize OrdWord))  => Int n r c -> Int n r c
abs i = bool i (negate i) (isNegative i)

instance (Symbolic c, KnownNat (n+1), KnownNat n, KnownRegisterSize r) => Scale Natural (Int n r c)
instance (Symbolic c, KnownNat (n+1), KnownNat n, KnownRegisterSize r) => Scale Integer (Int n r c)
instance (Symbolic c, KnownNat (n+1), KnownNat n, KnownRegisterSize r) => MultiplicativeMonoid (Int n r c) where
    one = fromConstant (1 :: Natural)

instance (Symbolic c, KnownNat (n+1), KnownNat n, KnownRegisterSize r) => AdditiveMonoid (Int n r c) where
    zero = fromConstant (0:: Natural)

instance (Symbolic c, KnownNat (n+1), KnownNat n, KnownRegisterSize r) => Semiring (Int n r c)
instance (Symbolic c, KnownNat (n+1), KnownRegisterSize r) => Arbitrary (Int n r c) where
    arbitrary = Int Haskell.<$> arbitrary

instance (Symbolic c, KnownNat n, k ~ n+1, KnownNat k, KnownRegisterSize r) => Iso (Int n r c) (UInt k r c) where
    from (Int u) = u

instance (Symbolic c, KnownNat n, k ~ n+1, KnownNat k, KnownRegisterSize r) => Iso (UInt k r c) (Int n r c) where
    from = Int

-- -- --------------------------------------------------------------------------------
--   AddInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTInteger
instance (Symbolic c, KnownNat (n+1), KnownRegisterSize r) => AdditiveSemigroup (Int n r c) where
    Int u1 + Int u2 = Int (u1+u2)

--   SubtractInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTInteger
instance (Symbolic c, KnownNat (n+1), KnownNat n, KnownRegisterSize r) => AdditiveGroup (Int n r c) where
    Int u1 - Int u2 = withDict (plusNat @n @1) $ Int (u1-u2)
    negate (Int u) = withDict (plusNat @n @1) $ Int (negate u)

--   MultiplyInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTInteger
instance (Symbolic c, KnownNat (n+1), KnownRegisterSize r) => MultiplicativeSemigroup (Int n r c) where
    Int x * Int y = Int $ x * y

--   DivideInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTInteger
--   ModInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTInteger
instance ( Symbolic c, KnownNat (n+1), KnownNat n, KnownRegisterSize r
         , KnownRegisters c (n+1) r
         , regSize ~ GetRegisterSize (BaseField c) (n+1) r
         , KnownNat (Ceil regSize OrdWord)
         ) => SemiEuclidean (Int n r c) where
  divMod i1 i2 = ifThenElse (isNegative i1 && isNegative i2) dm_mm ite_tf
    where
        (Int u1, Int u2) = (abs i1, abs i2)
        (d, m) = divMod u1 u2

        ite_tf = ifThenElse (isNegative i1 && not (isNegative i2)) dm_mp ite_pm
        ite_pm = ifThenElse (not (isNegative i1) && isNegative i2) dm_pm dm_pp

        dm_mm = (Int d, negate (Int m))
        dm_mp = (negate (Int d) - one, i2 - Int m)
        dm_pm = (negate (Int d) - one, i2 + Int m)
        dm_pp = (Int d, Int m)


div :: (Symbolic c, KnownNat (n + 1), KnownNat n, KnownRegisterSize r,
 KnownRegisters c (n + 1) r,
 regSize ~ GetRegisterSize (BaseField c) (n + 1) r,
 KnownNat (Ceil regSize OrdWord)) => Int n r c -> Int n r c -> Int n r c
div i1 i2 = Haskell.fst $ divMod i1 i2

mod :: (Symbolic c, KnownNat (n + 1), KnownNat n, KnownRegisterSize r,
 KnownRegisters c (n + 1) r,
 regSize ~ GetRegisterSize (BaseField c) (n + 1) r,
 KnownNat (Ceil regSize OrdWord)) => Int n r c -> Int n r c -> Int n r c
mod i1 i2 = Haskell.snd $ divMod i1 i2


--   QuotientInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTInteger
--   RemainderInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTInteger
quotRem :: ( Symbolic c, KnownNat (n+1), KnownNat n, KnownRegisterSize r
         , KnownRegisters c (n+1) r
         , regSize ~ GetRegisterSize (BaseField c) (n+1) r
         , KnownNat (Ceil regSize OrdWord)
         ) => Int n r c -> Int n r c -> (Int n r c, Int n r c)
quotRem i1 i2 = ifThenElse (isNegative i1 && isNegative i2) dm_mm ite_tf
    where
        (Int u1, Int u2) = (abs i1, abs i2)
        (d, m) = divMod u1 u2

        ite_tf = ifThenElse (isNegative i1 && isNotNegative i2) dm_mp ite_pm
        ite_pm = ifThenElse (isNotNegative i1 && isNegative i2) dm_pm dm_pp

        dm_mm = (Int d, negate (Int m))
        dm_mp = (negate (Int d), negate (Int m))
        dm_pm = (negate (Int d), Int m)
        dm_pp = (Int d, Int m)

--   EqualsInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTBool
--   LessThanInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTBool
--   LessThanEqualsInteger :: BuiltinIntegerFunction '[BTInteger, BTInteger] BTBool
instance ( Symbolic c, KnownNat (n+1), KnownNat n, KnownRegisterSize r
         , KnownRegisters c (n+1) r
         , regSize ~ GetRegisterSize (BaseField c) (n+1) r
         , KnownNat (Ceil regSize OrdWord)
         ) => Ord (Int n r c) where

    type OrderingOf (Int n r c) = Ordering c

    ordering x y z o = bool (bool x y (o == eq)) z (o == gt)

    compare x y = bool (bool lt eq (x == y)) gt (x > y)

    x <= y = y >= x

    x <  y = y > x

    i1@(Int u1) >= i2@(Int u2) = pm || (ub && (pp || nn))
        where
            ub = u1 >= u2
            nn = isNegative i1 && isNegative i2
            pp = isNotNegative i1 && isNotNegative i2
            pm = isNotNegative i1 && isNegative i2

    i1@(Int u1) > i2@(Int u2) = pm || (ub && (pp || nn))
        where
            ub = u1 > u2
            nn = isNegative i1 && isNegative i2
            pp = isNotNegative i1 && isNotNegative i2
            pm = isNotNegative i1 && isNegative i2



    max x y = bool @(Bool c) x y $ x < y

    min x y = bool @(Bool c) x y $ x > y