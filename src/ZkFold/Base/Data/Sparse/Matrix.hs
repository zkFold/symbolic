module ZkFold.Base.Data.Sparse.Matrix where

import Data.Map (Map)
import Data.Zip (Semialign (..), Zip (..))
import Test.QuickCheck (Arbitrary (..))
import ZkFold.Base.Algebra.Basic.Field (Zp)
import ZkFold.Base.Algebra.Basic.Number (KnownNat)
import Prelude hiding (length, sum, zip, zipWith, (*))

newtype SMatrix m n a = SMatrix {fromSMatrix :: Map (Zp m, Zp n) a}
  deriving (Show, Eq)

instance Foldable (SMatrix m n) where
  foldr f z (SMatrix as) = foldr f z as

instance Functor (SMatrix m n) where
  fmap f (SMatrix as) = SMatrix $ fmap f as

instance (KnownNat m, KnownNat n) => Semialign (SMatrix m n) where
  align (SMatrix as) (SMatrix bs) = SMatrix $ align as bs

  alignWith f (SMatrix as) (SMatrix bs) = SMatrix $ alignWith f as bs

instance (KnownNat m, KnownNat n) => Zip (SMatrix m n) where
  zip (SMatrix as) (SMatrix bs) = SMatrix $ zip as bs

  zipWith f (SMatrix as) (SMatrix bs) = SMatrix $ zipWith f as bs

instance (KnownNat m, KnownNat n, Arbitrary a) => Arbitrary (SMatrix m n a) where
  arbitrary = SMatrix <$> arbitrary
