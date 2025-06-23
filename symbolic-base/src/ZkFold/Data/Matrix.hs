{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module ZkFold.Data.Matrix where

import qualified Data.List as List
import Data.Maybe (fromJust)
import Data.These
import System.Random (Random (..))
import Test.QuickCheck (Arbitrary (..))
import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Data.Vector hiding (head, tail)
import Prelude hiding (Num (..), length, sum, zip, zipWith)

-- TODO: implement a proper matrix algebra
-- Could be useful for speeding up the proof computations

newtype Matrix m n a = Matrix (Vector m (Vector n a))
  deriving (Eq, Show)

toMatrix :: forall m n a. (KnownNat m, KnownNat n) => [[a]] -> Maybe (Matrix m n a)
toMatrix as = do
  as' <- mapM (toVector @n) as
  Matrix <$> toVector @m as'

fromMatrix :: forall m n a. Matrix m n a -> [[a]]
fromMatrix (Matrix as) = map fromVector $ fromVector as

transpose :: forall m n a. (KnownNat m, KnownNat n) => Matrix m n a -> Matrix n m a
transpose m = fromJust $ toMatrix @n @m $ List.transpose $ fromMatrix m

outer :: forall m n a b c. (a -> b -> c) -> Vector m a -> Vector n b -> Matrix m n c
outer f a b = Matrix $ fmap (\x -> fmap (f x) b) a

-- | Hadamard (entry-wise) matrix product
instance MultiplicativeSemigroup a => MultiplicativeSemigroup (Matrix m n a) where
  (*) = zipWith (*)

sum1 :: Semiring a => Matrix m n a -> Vector n a
sum1 (Matrix as) = Vector (sum <$> toV as)

sum2 :: (KnownNat m, KnownNat n, Semiring a) => Matrix m n a -> Vector m a
sum2 (Matrix as) = sum1 $ transpose $ Matrix as

matrixDotProduct :: forall m n a. Semiring a => Matrix m n a -> Matrix m n a -> a
matrixDotProduct a b = let Matrix m = a * b in sum $ fmap sum m

-- -- | Matrix multiplication
(.*.) :: (KnownNat n, KnownNat k, Semiring a) => Matrix m n a -> Matrix n k a -> Matrix m k a
a .*. b =
  let Matrix a' = a
      Matrix b' = transpose b
   in Matrix $ fmap (\x -> fmap (vectorDotProduct x) b') a'

instance Functor (Matrix m n) where
  fmap f (Matrix as) = Matrix $ fmap (fmap f) as

instance (KnownNat m, KnownNat n) => Applicative (Matrix m n) where
  pure a = Matrix $ pure $ pure a

  (Matrix fs) <*> (Matrix as) = Matrix $ zipWith (<*>) fs as

instance Semialign (Matrix m n) where
  align (Matrix as) (Matrix bs) = Matrix $ zipWith (zipWith These) as bs

  alignWith f (Matrix as) (Matrix bs) = Matrix $ zipWith (zipWith (\a b -> f $ These a b)) as bs

instance Zip (Matrix m n) where
  zip (Matrix as) (Matrix bs) = Matrix $ zipWith zip as bs

  zipWith f (Matrix as) (Matrix bs) = Matrix $ zipWith (zipWith f) as bs

deriving newtype instance (Arbitrary a, KnownNat m, KnownNat n) => Arbitrary (Matrix m n a)

deriving newtype instance (Random a, KnownNat m, KnownNat n) => Random (Matrix m n a)
