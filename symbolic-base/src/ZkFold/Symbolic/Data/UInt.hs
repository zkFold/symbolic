{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module ZkFold.Symbolic.Data.UInt where

import qualified Data.Aeson as Aeson
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.List (iterate)
import Data.Type.Bool (If)
import Data.Type.Ord (Max, Min, type (<=?))
import Data.Word (Word64)
import GHC.Real (toInteger)
import Test.QuickCheck (Arbitrary (..))
import Text.Show (Show)

import ZkFold.Algebra.Class
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq
import ZkFold.Prelude ((!!))
import ZkFold.Symbolic.Class (Arithmetic, Symbolic)
import ZkFold.Symbolic.Data.Bool (assert)
import ZkFold.Symbolic.Data.ByteString hiding (resize)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import ZkFold.Symbolic.Data.Ord
import ZkFold.Symbolic.Data.UIntData
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)

type family Search n b k where
  Search n b 0 = If (2 * n <=? b) (Max 1 n) (Search n b 1)
  Search _ _ 32 = 0
  Search n b k -- k is value of ceil(log2(n / regSize)), # of carry bits
    =
    If
      (k <=? Log2 n)
      ( If
          (2 * (Div (n - 1) (2 ^ k) + 1) + k <=? b)
          (Min (Div (b - k) 2) (Div (n - 1) (2 ^ (k - 1))))
          -- ceil((n + 2^k) / 2^k) <= ceil(2n / 2^k) = ceil(n / 2^(k - 1)),
          -- so @Div (n - 1) (2 ^ (k - 1))@ is in the right range.
          (Search n b (k + 1))
      )
      0

type RegisterSize n c = Search n (Log2 (Order c)) 0

newtype UInt (n :: Natural) c = UInt
  {uintData :: UIntData n (RegisterSize n c) c}
  deriving newtype (Collect (ConstrainedDatum c), Eq, Show)

type KnownUInt n c =
  (KnownUIntData n (RegisterSize n c), KnownNat (MaxAdded n (RegisterSize n c)))

instance SymbolicData (UInt n) where
  type Layout (UInt n) c = Layout (UIntData n (RegisterSize n c)) c
  type HasRep (UInt n) c = KnownUInt n c
  toLayout = toLayout . uintData
  fromLayout = UInt . fromLayout
  interpolate c = UInt . interpolate c . fmap (uintData <$>)

instance SymbolicInput (UInt n) where
  isValid = isValid . uintData

instance {-# OVERLAPPING #-} FromConstant (UInt n c) (UInt n c)

instance {-# OVERLAPPING #-} (KnownUInt n c, Symbolic c) => Scale (UInt n c) (UInt n c)

deriving newtype instance (KnownUInt n c, Symbolic c) => Arbitrary (UInt n c)

deriving newtype instance (KnownUInt n c, Symbolic c) => Ord (UInt n c)

deriving newtype instance
  FromConstant a (UIntData n (RegisterSize n c) c)
  => FromConstant a (UInt n c)

deriving newtype instance (KnownUInt n a, Arithmetic a) => ToConstant (UInt n a)

deriving newtype instance
  Scale a (UIntData n (RegisterSize n c) c) => Scale a (UInt n c)

deriving newtype instance
  (KnownUInt n c, Symbolic c) => AdditiveSemigroup (UInt n c)

deriving newtype instance (KnownUInt n c, Symbolic c) => Zero (UInt n c)

instance (KnownUInt n c, Symbolic c) => AdditiveMonoid (UInt n c)

deriving newtype instance
  (KnownUInt n c, Symbolic c) => AdditiveGroup (UInt n c)

deriving newtype instance
  (KnownUInt n c, Symbolic c) => MultiplicativeSemigroup (UInt n c)

deriving newtype instance
  (KnownUInt n c, Symbolic c) => MultiplicativeMonoid (UInt n c)

instance (KnownUInt n c, Symbolic c) => Semiring (UInt n c)

instance (KnownUInt n c, Symbolic c) => Ring (UInt n c)

deriving newtype instance
  (KnownUInt n c, Symbolic c) => SemiEuclidean (UInt n c)

deriving newtype instance
  (KnownUInt n c, Symbolic c) => Euclidean (UInt n c)

deriving newtype instance
  (KnownUInt n c, Symbolic c) => StrictNum (UInt n c)

instance (KnownUInt 64 c, Symbolic c) => FromConstant Word64 (UInt 64 c) where
  fromConstant = fromConstant . toInteger

instance (KnownUInt n c, Symbolic c) => Aeson.FromJSON (UInt n c) where
  parseJSON = fmap strictNatToUInt . Aeson.parseJSON @Natural

instance (KnownUInt n a, Arithmetic a) => Aeson.ToJSON (UInt n a) where
  toJSON = Aeson.toJSON . toConstant

instance MultiplicativeMonoid (UInt n c) => Exponent (UInt n c) Natural where
  (^) = natPow

resizeUInt :: (Symbolic c, KnownUInt m c, KnownUInt n c) => UInt m c -> UInt n c
resizeUInt = UInt . resizeUIntData . uintData

beBSToUInt :: (KnownUInt n c, Symbolic c) => ByteString n c -> UInt n c
beBSToUInt = UInt . beBSToUIntData

uintToBSbe :: (KnownUInt n c, Symbolic c) => UInt n c -> ByteString n c
uintToBSbe = uintDataToBSbe . uintData

feToUInt
  :: (KnownUInt (NumberOfBits c) c, Symbolic c)
  => FieldElement c -> UInt (NumberOfBits c) c
feToUInt = beBSToUInt . feToBSbe

uintToFE :: (Symbolic c, KnownUInt n c) => UInt n c -> FieldElement c
uintToFE = uintDataToFE . uintData

uintFromSemiEuclidean
  :: (SemiEuclidean a, FromConstant a c, KnownUInt n c) => a -> UInt n c
uintFromSemiEuclidean = UInt . uintDataFromSemiEuclidean

uintToIntegral :: (Symbolic c, KnownUInt n c) => UInt n c -> IntegralOf c
uintToIntegral = uintDataToIntegral . uintData

strictNatToUInt
  :: forall n c. (KnownUInt n c, Symbolic c) => Natural -> UInt n c
strictNatToUInt x =
  assert (\_ -> fromConstant (x < 2 ^ value @n)) (fromConstant x)

strictZpToUInt :: (KnownUInt n c, Symbolic c) => Zp p -> UInt n c
strictZpToUInt = strictNatToUInt . toConstant

strictFEToUInt :: (KnownUInt n c, Symbolic c) => FieldElement c -> UInt n c
strictFEToUInt = UInt . strictFEToUIntData

--------------------------------------------------------------------------------

bitsPow
  :: forall c n p
   . (KnownUInt n c, KnownNat p, Symbolic c)
  => Natural
  -> ByteString p c
  -> UInt n c
  -> UInt n c
  -> UInt n c
  -> UInt n c
bitsPow 0 _ res _ _ = res
bitsPow b bits res n m = bitsPow (b -! 1) bits newRes sq m
 where
  sq = (n *! n) `mod` m
  newRes = ifThenElse (isSet bits (b -! 1)) ((res *! n) `mod` m) res

-- | @expMod n pow modulus@ calculates @n^pow % modulus@
-- where all values are arithmetised
expMod
  :: forall c n p m
   . Symbolic c
  => KnownUInt p c
  => KnownUInt (2 * m) c
  => KnownUInt m c
  => KnownUInt n c
  => UInt n c
  -> UInt p c
  -> UInt m c
  -> UInt m c
expMod n pow modulus = resizeUInt result
 where
  m' :: UInt (2 * m) c
  m' = resizeUInt modulus

  result :: UInt (2 * m) c
  result = bitsPow (value @p) (uintToBSbe pow) one (resizeUInt n `mod` m') m'

-- | n ^ 65537 `mod` modulus
exp65537Mod
  :: forall c n m
   . (KnownUInt (2 * m) c, KnownUInt m c, KnownUInt n c, Symbolic c)
  => UInt n c
  -> UInt m c
  -> UInt m c
exp65537Mod n modulus = resizeUInt $ (sq_2_16 *! n') `mod` m'
 where
  m' :: UInt (2 * m) c
  m' = resizeUInt modulus

  n' :: UInt (2 * m) c
  n' = resizeUInt n

  sq_2_16 = iterate (\x -> (x *! x) `mod` m') n' !! (16 :: Natural)
