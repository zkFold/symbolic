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

import           ZkFold.Base.Algebra.Basic.Class  hiding (Euclidean (..))
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Data.Vector          (Vector (..), fromVector)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Class       (SymbolicData)
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.Ord
import           ZkFold.Symbolic.Data.UInt
import           ZkFold.Symbolic.Interpreter      (Interpreter (..))


newtype Int (n :: Natural) (r :: RegisterSize) (c :: (Type -> Type) -> Type) = Int { uint :: UInt n r c}

deriving instance Generic (Int n r c)
deriving instance (NFData (c (Vector (NumberOfRegisters (BaseField c) n r)))) => NFData (Int n r c)
deriving instance (Haskell.Eq (c (Vector (NumberOfRegisters (BaseField c) n r)))) => Haskell.Eq (Int n r c)
deriving instance (Haskell.Show (BaseField c), Haskell.Show (c (Vector (NumberOfRegisters (BaseField c) n r))))
    => Haskell.Show (Int n r c)
deriving instance (KnownRegisters c n r, Symbolic c) => SymbolicData (Int n r c)
deriving instance (KnownRegisters c n r, Symbolic c) => Conditional (Bool c) (Int n r c)
deriving instance (KnownRegisters c n r, Symbolic c) => Eq (Int n r c)
deriving newtype instance (Symbolic c, KnownNat n, KnownRegisterSize r) => FromConstant Natural (Int n r c)
deriving newtype instance (Symbolic c, KnownNat n, KnownRegisterSize r) => FromConstant Integer (Int n r c)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Exponent (Int n r c) Natural where
    (^) = natPow

------------------------------------------------------------------------------

instance
    ( Arithmetic a, KnownRegisterSize r, KnownNat n) => ToConstant (Int n r (Interpreter a)) where
    type Const (Int n r (Interpreter a)) = Integer
    toConstant i@(Int u) =
        withGetRegisterSize @n @r @a $
            Haskell.bool (Haskell.toInteger $ toConstant u) (negate . Haskell.toInteger . toConstant $ negate u) (isNegative i Haskell.== true)

isNegative :: forall n r c. (Symbolic c, KnownNat n, KnownRegisterSize r) => Int n r c -> Bool c
isNegative (Int (UInt u)) = Bool $ fromCircuitF u $ \regs -> do
    let hd = Haskell.last $ fromVector regs
    (_, h) <- splitExpansion (highRegisterSize @(BaseField c) @n @r -! 1) 1 hd
    Haskell.return $ Par1 h

isNotNegative :: forall n r c. (Symbolic c, KnownNat n, KnownRegisterSize r) => Int n r c -> Bool c
isNotNegative i = not (isNegative i)

abs :: forall c n r. (Symbolic c, KnownNat n, KnownRegisterSize r) => Int n r c -> Int n r c
abs i = withNumberOfRegisters @n @r @(BaseField c) $ bool i (negate i) (isNegative i)

deriving newtype instance (Symbolic c, KnownNat n, KnownRegisterSize r) => MultiplicativeMonoid (Int n r c)
deriving newtype instance (Symbolic c, KnownNat n, KnownRegisterSize r) => AdditiveMonoid (Int n r c)
deriving newtype instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Arbitrary (Int n r c)
instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Scale Natural (Int n r c)
instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Scale Integer (Int n r c)
instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Semiring (Int n r c)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Iso (Int n r c) (UInt n r c) where
    from (Int u) = u

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Iso (UInt n r c) (Int n r c) where
    from = Int

------------------------------------------------------------------------------
instance (Symbolic c, KnownNat n, KnownRegisterSize r) => AdditiveSemigroup (Int n r c) where
    Int u1 + Int u2 = Int (u1+u2)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => AdditiveGroup (Int n r c) where
    Int u1 - Int u2 = Int (u1-u2)
    negate (Int u) = Int (negate u)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => MultiplicativeSemigroup (Int n r c) where
    Int x * Int y = Int $ x * y

instance ( Symbolic c, KnownNat n, KnownRegisterSize r
         , KnownRegisters c n r
         ) => SemiEuclidean (Int n r c) where
  divMod i1 i2 = ifThenElse (i1 == zero) dm_pp ite_nn
    where
        (Int u1, Int u2) = (abs i1, abs i2)
        (d, m) = withGetRegisterSize @n @r @(BaseField c) $
                    withCeilRegSize @(GetRegisterSize (BaseField c) n r) @OrdWord $
                        divMod u1 u2

        ite_nn = ifThenElse (isNegative i1 && isNegative i2) dm_mm ite_np
        ite_np = ifThenElse (isNegative i1 && isNotNegative i2) dm_mp ite_pm
        ite_pm = ifThenElse (isNotNegative i1 && isNegative i2) dm_pm dm_pp

        dm_mm = (Int d, negate (Int m))
        dm_mp = bool (negate (Int d) - one, i2 - Int m) (negate $ Int d, Int m) (m == zero)
        dm_pm = bool (negate (Int d) - one, i2 + Int m) (negate $ Int d, Int m) (m == zero)
        dm_pp = (Int d, Int m)


div :: forall n r c.
    ( Symbolic c, KnownNat n, KnownRegisterSize r
    , KnownRegisters c n r
    ) => Int n r c -> Int n r c -> Int n r c
div i1 i2 = Haskell.fst $ divMod i1 i2

mod :: forall n r c.
    ( Symbolic c, KnownNat n, KnownRegisterSize r
    , KnownRegisters c n r
    ) => Int n r c -> Int n r c -> Int n r c
mod i1 i2 = Haskell.snd $ divMod i1 i2


quotRem :: forall n r c.
    ( Symbolic c, KnownNat n, KnownRegisterSize r
    , KnownRegisters c n r
    ) => Int n r c -> Int n r c -> (Int n r c, Int n r c)
quotRem i1 i2 = ifThenElse (isNegative i1 && isNegative i2) dm_mm ite_tf
    where
        (Int u1, Int u2) = (abs i1, abs i2)
        (d, m) = withGetRegisterSize @n @r @(BaseField c) $
                    withCeilRegSize @(GetRegisterSize (BaseField c) n r) @OrdWord $
                        divMod u1 u2

        ite_tf = ifThenElse (isNegative i1 && isNotNegative i2) dm_mp ite_pm
        ite_pm = ifThenElse (isNotNegative i1 && isNegative i2) dm_pm dm_pp

        dm_mm = (Int d, negate (Int m))
        dm_mp = (negate (Int d), negate (Int m))
        dm_pm = (negate (Int d), Int m)
        dm_pp = (Int d, Int m)

quot :: forall n r c. (Symbolic c, KnownNat n, KnownRegisterSize r, KnownRegisters c n r
    ) => Int n r c -> Int n r c -> Int n r c
quot i1 i2 = Haskell.fst $ quotRem i1 i2

rem :: forall n r c.
    ( Symbolic c, KnownNat n, KnownRegisterSize r
    , KnownRegisters c n r
    ) => Int n r c -> Int n r c -> Int n r c
rem i1 i2 = Haskell.snd $ quotRem i1 i2


instance
    ( Symbolic c, KnownNat n, KnownRegisterSize r
    , KnownRegisters c n r
    ) => Ord (Int n r c) where

    type OrderingOf (Int n r c) = Ordering c

    ordering x y z o = bool (bool x y (o == eq)) z (o == gt)

    compare x y = bool (bool lt eq (x == y)) gt (x > y)

    x <= y = y >= x

    x <  y = y > x

    i1 >= i2 = (isNotNegative i1 && isNegative i2) || (ub && (not $ xor (isNegative i1) (isNegative i2)))
        where
            ub = withGetRegisterSize @n @r @(BaseField c) $ withCeilRegSize @(GetRegisterSize (BaseField c) n r) @OrdWord $
                 uint i1 >= uint i2

    i1 > i2 = (isNotNegative i1 && isNegative i2) || (ub && (not $ xor (isNegative i1) (isNegative i2)))
        where
            ub = withGetRegisterSize @n @r @(BaseField c) $ withCeilRegSize @(GetRegisterSize (BaseField c) n r) @OrdWord $
                 uint i1 > uint i2


    max x y = bool @(Bool c) x y $ x < y

    min x y = bool @(Bool c) x y $ x > y
