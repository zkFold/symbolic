{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Data.Vector (
  module ZkFold.Data.Vector,
  module Data.Zip,
) where

import Control.Applicative (Applicative, pure)
import Control.DeepSeq (NFData, NFData1)
import Control.Monad (Monad)
import Control.Monad.State.Strict (runState, state)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Bool (otherwise)
import Data.Constraint.Nat (Max)
import Data.Distributive (Distributive (..))
import Data.Foldable (Foldable, fold)
import Data.Function (const, ($), (.))
import Data.Functor (Functor, fmap, (<$>))
import Data.Functor.Classes (Eq1, Show1)
import Data.Functor.Rep (
  Representable (..),
  collectRep,
  distributeRep,
  mzipRep,
  mzipWithRep,
  pureRep,
 )
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Data.These (These (..))
import Data.Traversable (Traversable, sequenceA, traverse)
import Data.Tuple (fst, snd, uncurry)
import qualified Data.Vector as V
import Data.Vector.Binary ()
import qualified Data.Vector.Split as V
import Data.Zip (Semialign (..), Unzip (..), Zip (..))
import GHC.Generics (Generic)
import GHC.IsList (IsList (..))
import System.Random (Random (..))
import Test.QuickCheck (Arbitrary (..), Arbitrary1 (..), arbitrary1)
import Text.Show (Show)
import Prelude (Integer)
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.Field
import ZkFold.Algebra.Number
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Bool
import ZkFold.Data.ByteString (Binary (..))
import ZkFold.Data.Eq
import ZkFold.Prelude (length)

newtype Vector (size :: Natural) a = Vector {toV :: V.Vector a}
  deriving (Eq1, Foldable, Functor, Generic, NFData, NFData1, P.Eq, P.Ord, Show, Show1, Traversable)
  deriving newtype (FromJSON, ToJSON)

instance (Conditional bool x, KnownNat n) => Conditional bool (Vector n x) where
  bool fv tv b = mzipWithRep (\f t -> bool f t b) fv tv

instance (Eq x, KnownNat n) => Eq (Vector n x) where
  type BooleanOf (Vector n x) = BooleanOf x
  u == v = V.foldl (&&) true (V.zipWith (==) (toV u) (toV v))
  u /= v = V.foldl (||) false (V.zipWith (/=) (toV u) (toV v))

instance KnownNat size => Representable (Vector size) where
  type Rep (Vector size) = Zp size
  index (Vector v) ix = v V.! P.fromIntegral (fromZp ix)
  tabulate f = Vector (V.generate (integral @size) (f . P.fromIntegral))

instance KnownNat size => Distributive (Vector size) where
  distribute = distributeRep
  collect = collectRep

vtoVector :: forall size a. KnownNat size => V.Vector a -> Maybe (Vector size a)
vtoVector as
  | V.length as == integral @size = Just $ Vector as
  | otherwise = Nothing

instance IsList (Vector n a) where
  type Item (Vector n a) = a
  toList = fromVector
  fromList = unsafeToVector

toVector :: forall size a. KnownNat size => [a] -> Maybe (Vector size a)
toVector as
  | length as == value @size = Just $ Vector (V.fromList as)
  | otherwise = Nothing

unsafeToVector :: forall size a. [a] -> Vector size a
unsafeToVector = Vector . V.fromList

unfold :: forall size a b. KnownNat size => (b -> (a, b)) -> b -> Vector size a
unfold f = Vector . V.unfoldrExactN (integral @size) f

fromVector :: Vector size a -> [a]
fromVector (Vector !as) = V.toList as

(!!) :: Vector size a -> Natural -> a
(Vector as) !! i = as V.! P.fromIntegral i

uncons :: Vector size a -> (a, Vector (size - 1) a)
uncons (Vector !lst) = (V.head lst, Vector $ V.tail lst)

reverse :: Vector size a -> Vector size a
reverse (Vector !lst) = Vector (V.reverse lst)

head :: Vector size a -> a
head (Vector !as) = V.head as

last :: Vector size a -> a
last (Vector !as) = V.last as

tail :: Vector size a -> Vector (size - 1) a
tail (Vector !as) = Vector $ V.tail as

init :: Vector size a -> Vector (size - 1) a
init (Vector !as) = Vector $ V.init as

scanl :: forall size a b. (b -> a -> b) -> b -> Vector size a -> Vector (size + 1) b
scanl f z (Vector !as) = Vector $ V.scanl f z as

singleton :: a -> Vector 1 a
singleton = Vector . pure

item :: Vector 1 a -> a
item = head

mapWithIx :: forall n a b. KnownNat n => (Natural -> a -> b) -> Vector n a -> Vector n b
mapWithIx f (Vector !l) = Vector $ V.zipWith f (V.enumFromTo 0 (value @n -! 1)) l

enumerate :: forall n a. KnownNat n => Vector n a -> Vector n (Natural, a)
enumerate = mapWithIx (,)

mapMWithIx :: forall n m a b. (KnownNat n, Monad m) => (Natural -> a -> m b) -> Vector n a -> m (Vector n b)
mapMWithIx f (Vector !l) = Vector <$> V.zipWithM f (V.enumFromTo 0 (value @n -! 1)) l

-- TODO: Check that n <= size?
take :: forall n size a. KnownNat n => Vector size a -> Vector n a
take (Vector !lst) = Vector (V.take (integral @n) lst)

drop :: forall n m a. KnownNat n => Vector (n + m) a -> Vector m a
drop (Vector !lst) = Vector (V.drop (integral @n) lst)

splitAt :: forall n m a. KnownNat n => Vector (n + m) a -> (Vector n a, Vector m a)
splitAt (Vector !lst) = (Vector (V.take (integral @n) lst), Vector (V.drop (integral @n) lst))

rotate :: forall size a. KnownNat size => Vector size a -> Integer -> Vector size a
rotate (Vector !lst) n = Vector (r <> l)
 where
  len :: Integer
  len = P.fromIntegral $ value @size

  lshift :: Int
  lshift = P.fromIntegral $ n `mod` len

  (!l, !r) = V.splitAt lshift lst

shift :: forall size a. KnownNat size => Vector size a -> Integer -> a -> Vector size a
shift (Vector lst) n pad
  | n P.< 0 = Vector $ V.take (integral @size) (padList <> lst)
  | otherwise = Vector $ V.drop (P.fromIntegral n) (lst <> padList)
 where
  padList = V.replicate (P.fromIntegral $ P.abs n) pad

vectorDotProduct :: forall size a. Semiring a => Vector size a -> Vector size a -> a
vectorDotProduct (Vector !as) (Vector !bs) = sum $ zipWith (*) as bs

empty :: Vector 0 a
empty = Vector V.empty

infixr 5 .:

(.:) :: a -> Vector n a -> Vector (n + 1) a
a .: (Vector !lst) = Vector (a `V.cons` lst)

append :: Vector m a -> Vector n a -> Vector (m + n) a
append (Vector !l) (Vector !r) = Vector (l <> r)

concat :: Vector m (Vector n a) -> Vector (m * n) a
concat = Vector . V.concatMap toV . toV

unsafeConcat :: forall m n a. [Vector n a] -> Vector (m * n) a
unsafeConcat = concat . unsafeToVector @m

-- | Map a function over a vector and concatenate the results.
concatMap :: forall m n a b. (a -> Vector n b) -> Vector m a -> Vector (m * n) b
concatMap f (Vector v) = Vector $ V.concatMap (toV . f) v

chunks :: forall m n a. KnownNat n => Vector (m * n) a -> Vector m (Vector n a)
chunks (Vector vectors) = unsafeToVector (Vector <$> V.chunksOf (P.fromIntegral $ value @n) vectors)

-- | Slice a vector of size @size@, starting at index @i@, and taking @n@ elements.
--
-- Note that we'll get run-time error if @i + n > size@.
slice
  :: forall i n size a
   . KnownNat i
  => KnownNat n
  => Vector size a
  -> Vector n a
slice (Vector v) = Vector $ V.slice (P.fromIntegral $ value @i) (P.fromIntegral $ value @n) v

-- | Yield the vector obtained by replacing each element @i@ of the
-- index vector by @xs'!'i@.
backpermute :: forall n m a. Vector n a -> Vector m (Zp n) -> Vector m a
backpermute (Vector v) (Vector is) = Vector $ V.backpermute v $ V.map (P.fromIntegral . fromZp) is

instance (Binary a, KnownNat n) => Binary (Vector n a) where
  put = fold . V.map put . toV
  get = Vector <$> V.replicateM (integral @n) get

instance KnownNat size => Applicative (Vector size) where
  pure a = Vector $ V.replicate (integral @size) a

  (Vector !fs) <*> (Vector !as) = Vector $ V.zipWith ($) fs as

instance Semialign (Vector size) where
  align (Vector !as) (Vector !bs) = Vector $ V.zipWith These as bs

-- alignRight [1] [2, 3] == [That 2, These 1 3]
-- [   1]
-- [2, 3]
--
alignRight :: forall m n a b. Vector m a -> Vector n b -> Vector (Max m n) (These a b)
alignRight !v1 !v2 = unsafeToVector $ P.reverse $ align (P.reverse $ fromVector v1) (P.reverse $ fromVector v2)

-- alignLeft [1] [2, 3] == [These 1 2, That 3]
-- [1   ]
-- [2, 3]
--
alignLeft :: forall m n a b. Vector m a -> Vector n b -> Vector (Max m n) (These a b)
alignLeft !v1 !v2 = unsafeToVector $ align (fromVector v1) (fromVector v2)

instance Zip (Vector size) where
  zip (Vector !as) (Vector !bs) = Vector $ V.zip as bs

  zipWith f (Vector !as) (Vector !bs) = Vector $ V.zipWith f as bs

instance Unzip (Vector size) where
  unzip !v = (fst <$> v, snd <$> v)

instance (Arbitrary a, KnownNat size) => Arbitrary (Vector size a) where
  arbitrary = arbitrary1

instance KnownNat size => Arbitrary1 (Vector size) where
  liftArbitrary = sequenceA . pureRep

instance (KnownNat size, Random a) => Random (Vector size a) where
  random = runState (sequenceA (pureRep (state random)))
  randomR = runState . traverse (state . randomR) . uncurry mzipRep

-------------------------------------------------- Algebraic instances --------------------------------------------------

instance AdditiveSemigroup a => AdditiveSemigroup (Vector n a) where
  (+) = zipWith (+)

instance Scale b a => Scale b (Vector n a) where
  scale = fmap . scale

instance (AdditiveMonoid a, KnownNat n) => AdditiveMonoid (Vector n a) where
  zero = tabulate (const zero)

instance (AdditiveGroup a, KnownNat n) => AdditiveGroup (Vector n a) where
  negate = fmap negate

  (-) = zipWith (-)
