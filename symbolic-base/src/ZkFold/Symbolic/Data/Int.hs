{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module ZkFold.Symbolic.Data.Int where

import           Control.DeepSeq
import qualified Data.Bool                        as Haskell
import           Data.Kind                        (Type)
import           GHC.Generics                     (Generic, Par1 (..))
import           Prelude                          (Integer, ($), (.))
import qualified Prelude                          as Haskell hiding ((-))
import           Test.QuickCheck                  (Arbitrary (..))

import           ZkFold.Algebra.Class             hiding (Euclidean (..))
import           ZkFold.Algebra.Number
import           ZkFold.Data.HFunctor.Classes     (HEq, HNFData, HShow)
import           ZkFold.Data.Vector               (fromVector)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Class       (SymbolicData)
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.Input       (SymbolicInput)
import           ZkFold.Symbolic.Data.Ord
import           ZkFold.Symbolic.Data.UInt
import           ZkFold.Symbolic.Interpreter      (Interpreter (..))


newtype Int (r :: Natural) (n :: Natural) (c :: (Type -> Type) -> Type) = Int { uint :: UInt r n c}

deriving instance Generic (Int r n c)
deriving instance HNFData c => NFData (Int r n c)
deriving instance HEq c => Haskell.Eq (Int r n c)
deriving instance HShow c => Haskell.Show (Int r n c)
deriving instance (KnownNat n, Symbolic c) => SymbolicData (Int r n c)
deriving instance (KnownNat r, KnownNat n, Symbolic c) => SymbolicInput (Int r n c)
deriving instance (KnownNat n, Symbolic c) => Conditional (Bool c) (Int r n c)
deriving instance (KnownNat n, Symbolic c) => Eq (Int r n c)
deriving newtype instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => FromConstant Natural (Int r n c)
deriving newtype instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => FromConstant Integer (Int r n c)

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => Exponent (Int r n c) Natural where
    (^) = natPow

-- ------------------------------------------------------------------------------

instance
    ( KnownNat r
    , KnownNat n
    , Arithmetic a
    , IsValidRegister r n (Interpreter a)
    ) => ToConstant (Int r n (Interpreter a)) where
    type Const (Int r n (Interpreter a)) = Integer
    toConstant i@(Int u) =
            Haskell.bool (Haskell.toInteger $ toConstant u) (negate . Haskell.toInteger . toConstant $ negate u) (isNegative i Haskell.== true)

isNegative :: forall r n c. (KnownNat r, KnownNat n, Symbolic c) => Int r n c -> Bool c
isNegative (Int (UInt u)) = Bool $ fromCircuitF u $ \regs -> do
    let hd = Haskell.last $ fromVector regs
    (_, h) <- splitExpansion (highRegisterSize @r @n -! 1) 1 hd
    Haskell.return $ Par1 h

isNotNegative :: forall r n c. (KnownNat r, KnownNat n, Symbolic c) => Int r n c -> Bool c
isNotNegative i = not (isNegative i)

abs :: forall r n c . (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => Int r n c -> Int r n c
abs i = bool i (negate i) (isNegative i)

deriving newtype instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => MultiplicativeMonoid (Int r n c)
deriving newtype instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => AdditiveMonoid (Int r n c)
deriving newtype instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => Arbitrary (Int r n c)
instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => Scale Natural (Int r n c)
instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => Scale Integer (Int r n c)
instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => Semiring (Int r n c)

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => Iso (Int r n c) (UInt r n c) where
    from (Int u) = u

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => Iso (UInt r n c) (Int r n c) where
    from = Int

-- ------------------------------------------------------------------------------
instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => AdditiveSemigroup (Int r n c) where
    Int u1 + Int u2 = Int (u1+u2)

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => AdditiveGroup (Int r n c) where
    Int u1 - Int u2 = Int (u1-u2)
    negate (Int u) = Int (negate u)

instance (KnownNat r, KnownNat n, Symbolic c) => MultiplicativeSemigroup (Int r n c) where
    Int x * Int y = Int $ x * y

instance
    ( KnownNat r
    , KnownNat n
    , Symbolic c
    , IsValidRegister r n c
    ) => SemiEuclidean (Int r n c) where
  divMod i1 i2 = ifThenElse (i1 == zero) dm_pp ite_nn
    where
        (Int u1, Int u2) = (abs i1, abs i2)
        (d, m) = divMod u1 u2

        ite_nn = ifThenElse (isNegative i1 && isNegative i2) dm_mm ite_np
        ite_np = ifThenElse (isNegative i1 && isNotNegative i2) dm_mp ite_pm
        ite_pm = ifThenElse (isNotNegative i1 && isNegative i2) dm_pm dm_pp

        dm_mm = (Int d, negate (Int m))
        dm_mp = bool (negate (Int d) - one, i2 - Int m) (negate $ Int d, Int m) (m == zero)
        dm_pm = bool (negate (Int d) - one, i2 + Int m) (negate $ Int d, Int m) (m == zero)
        dm_pp = (Int d, Int m)


div :: forall r n c .
    ( KnownNat r
    , KnownNat n
    , Symbolic c
    , IsValidRegister r n c
    ) => Int r n c -> Int r n c -> Int r n c
div i1 i2 = Haskell.fst $ divMod i1 i2

mod :: forall r n c.
    ( KnownNat r
    , KnownNat n
    , Symbolic c
    , IsValidRegister r n c
    ) => Int r n c -> Int r n c -> Int r n c
mod i1 i2 = Haskell.snd $ divMod i1 i2


quotRem :: forall r n c .
    ( KnownNat r
    , KnownNat n
    , Symbolic c
    , IsValidRegister r n c
    ) => Int r n c -> Int r n c -> (Int r n c, Int r n c)
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

quot :: forall r n c .
    ( KnownNat r
    , KnownNat n
    , Symbolic c
    , IsValidRegister r n c
    ) => Int r n c -> Int r n c -> Int r n c
quot i1 i2 = Haskell.fst $ quotRem i1 i2

rem :: forall r n c .
    ( KnownNat r
    , KnownNat n
    , Symbolic c
    , IsValidRegister r n c
    ) => Int r n c -> Int r n c -> Int r n c
rem i1 i2 = Haskell.snd $ quotRem i1 i2


instance
    ( KnownNat r
    , KnownNat n
    , Symbolic c
    , IsValidRegister r n c
    ) => Ord (Int r n c) where

    type OrderingOf (Int r n c) = Ordering c

    ordering x y z o = bool (bool x y (o == eq)) z (o == gt)

    compare x y = bool (bool lt eq (x == y)) gt (x > y)

    x <= y = y >= x

    x <  y = y > x

    i1 >= i2 = (isNotNegative i1 && isNegative i2) || (ub && (not $ xor (isNegative i1) (isNegative i2)))
        where
            ub = uint i1 >= uint i2

    i1 > i2 = (isNotNegative i1 && isNegative i2) || (ub && (not $ xor (isNegative i1) (isNegative i2)))
        where
            ub = uint i1 > uint i2


    max x y = bool @(Bool c) x y $ x < y

    min x y = bool @(Bool c) x y $ x > y
