module ZkFold.Data.Sparse.Vector where

import Data.Map (Map, empty, filter, fromList, map, toList)
import Data.These (These (..))
import Data.Zip (Semialign (..), Zip (..))
import Test.QuickCheck (Arbitrary (..))
import Prelude hiding (Num (..), filter, length, map, sum, zip, zipWith, (/))

import ZkFold.Algebra.Class
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Data.ByteString (Binary (..))

newtype SVector size a = SVector {fromSVector :: Map (Zp size) a}
  deriving (Eq, Show)

instance (Binary a, KnownNat n) => Binary (SVector n a) where
  put = put . toList . fromSVector
  get = SVector . fromList <$> get

instance Foldable (SVector size) where
  foldr f z (SVector as) = foldr f z as

instance Functor (SVector size) where
  fmap f (SVector as) = SVector $ fmap f as

instance KnownNat size => Semialign (SVector size) where
  align (SVector as) (SVector bs) = SVector $ align as bs

  alignWith f (SVector as) (SVector bs) = SVector $ alignWith f as bs

instance KnownNat size => Zip (SVector size) where
  zip (SVector as) (SVector bs) = SVector $ zip as bs

  zipWith f (SVector as) (SVector bs) = SVector $ zipWith f as bs

instance (Arbitrary a, KnownNat size) => Arbitrary (SVector size a) where
  arbitrary = SVector <$> arbitrary

instance (AdditiveMonoid a, Eq a, KnownNat size) => AdditiveSemigroup (SVector size a) where
  va + vb =
    SVector $
      filter (/= zero) $
        fromSVector $
          alignWith
            ( \case
                This a -> a
                That b -> b
                These a b -> a + b
            )
            va
            vb

(.+) :: (AdditiveMonoid a, Eq a, KnownNat size) => SVector size a -> SVector size a -> SVector size a
(.+) = (+)

instance Scale c a => Scale c (SVector size a) where
  scale c (SVector as) = SVector (map (scale c) as)

instance (AdditiveMonoid a, Eq a, KnownNat size) => AdditiveMonoid (SVector size a) where
  zero = SVector empty

instance (AdditiveGroup a, Eq a, KnownNat size) => AdditiveGroup (SVector size a) where
  negate = fmap negate

(.-) :: (AdditiveGroup a, Eq a, KnownNat size) => SVector size a -> SVector size a -> SVector size a
(.-) = (-)

(.*) :: (KnownNat size, MultiplicativeSemigroup a) => SVector size a -> SVector size a -> SVector size a
(.*) = zipWith (*)

(./) :: (KnownNat size, MultiplicativeGroup a) => SVector size a -> SVector size a -> SVector size a
(./) = zipWith (/)
