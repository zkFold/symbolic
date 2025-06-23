{-# LANGUAGE BlockArguments #-}

module ZkFold.Algebra.Polynomial.Univariate.Simple (
  SimplePoly (coeffs),
  fromVector,
  toVector,
) where

import qualified Data.Eq as Haskell
import Data.Function (on, ($), (.))
import Data.Functor (fmap, (<$>))
import Data.List (zip)
import Data.Ord (min, (<))
import Data.Semialign (alignWith)
import qualified Data.These as T
import Data.Tuple (uncurry)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified GHC.Num as Int
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary (..))
import Text.Show (Show)

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (KnownNat, integral)
import ZkFold.Algebra.Polynomial.Univariate
import qualified ZkFold.Data.Vector as ZkFold

-- | A "simple" polynomial-vector type, meaning that
-- it uses a simple multiplication algorithm.
--
-- Not particularly efficient, but still usable in Symbolic
-- since its operations do not require 'Prelude.Eq'.
newtype SimplePoly a (n :: Natural) = SimplePoly
  { coeffs :: V.Vector a
  -- ^ Vector of coefficients in ascending-degree order.
  --
  --   NOTE there is no guarantee that length of 'coeffs' is @n@,
  --   only that its length is _not greater than_ @n@.
  --   To get correctly sized vector, use 'toVector'.
  --
  --   NOTE since 'SimplePoly' does not use 'Prelude.Eq', it is possible that
  --   there are some non-zeroed greater coefficients. Use with care.
  }
  deriving Show

instance
  (AdditiveMonoid a, Haskell.Eq a, KnownNat n)
  => Haskell.Eq (SimplePoly a n)
  where
  (==) = (Haskell.==) `on` toVector

fromVector :: forall a n. ZkFold.Vector n a -> SimplePoly a n
fromVector (ZkFold.Vector cs) = SimplePoly cs

-- | Vector of exactly @n@ coefficients in ascending-degree order.
toVector
  :: forall a n
   . (AdditiveMonoid a, KnownNat n)
  => SimplePoly a n -> ZkFold.Vector n a
toVector (SimplePoly cs) =
  ZkFold.Vector $ cs V.++ V.replicate (integral @n Int.- V.length cs) zero

instance {-# OVERLAPPING #-} FromConstant (SimplePoly a n) (SimplePoly a n)

instance FromConstant k a => FromConstant k (SimplePoly a n) where
  fromConstant = SimplePoly . V.singleton . fromConstant

instance {-# OVERLAPPING #-} (Semiring a, KnownNat n) => Scale (SimplePoly a n) (SimplePoly a n)

instance Scale k a => Scale k (SimplePoly a n) where
  scale k = SimplePoly . fmap (scale k) . coeffs

instance (Semiring a, KnownNat n) => Exponent (SimplePoly a n) Natural where
  (^) = natPow

instance AdditiveSemigroup a => AdditiveSemigroup (SimplePoly a n) where
  SimplePoly p + SimplePoly q = SimplePoly $ alignWith (T.mergeThese (+)) p q

instance AdditiveMonoid a => AdditiveMonoid (SimplePoly a n) where
  zero = SimplePoly V.empty

instance AdditiveGroup a => AdditiveGroup (SimplePoly a n) where
  negate = SimplePoly . fmap negate . coeffs

instance (Semiring a, KnownNat n) => MultiplicativeSemigroup (SimplePoly a n) where
  SimplePoly p * SimplePoly q =
    let pLen = V.length p
        qLen = V.length q
        pqLen = case pLen Int.+ qLen of
          0 -> 0
          l -> min (integral @n) (l Int.- 1)
     in SimplePoly $ V.generate pqLen \k ->
          sum
            [ (p V.! i) * (q V.! j)
            | i <- [0 .. min (pLen Int.- 1) k]
            , let j = k Int.- i
            , j < qLen
            ]

instance (Semiring a, KnownNat n) => MultiplicativeMonoid (SimplePoly a n) where
  one = fromConstant (one :: a)

instance (Semiring a, KnownNat n) => Semiring (SimplePoly a n)

instance (Ring a, KnownNat n) => Ring (SimplePoly a n)

instance Ring a => UnivariateRingPolyVec a (SimplePoly a) where
  SimplePoly p .*. SimplePoly q =
    SimplePoly $ alignWith (uncurry (*) . T.fromThese zero zero) p q
  (+.) c = SimplePoly . ZkFold.toV . fmap (c +) . toVector
  toPolyVec :: forall n. KnownNat n => Vector a -> SimplePoly a n
  toPolyVec = SimplePoly . V.take (integral @n)
  fromPolyVec = coeffs
  evalPolyVec (SimplePoly p) x =
    sum [c * x ^ i | (i, c) <- zip [(0 :: Natural) ..] (V.toList p)]

instance (Arbitrary a, KnownNat n) => Arbitrary (SimplePoly a n) where
  arbitrary = fromVector <$> arbitrary
