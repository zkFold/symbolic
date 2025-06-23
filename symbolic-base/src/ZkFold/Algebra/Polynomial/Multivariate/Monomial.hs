{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}

module ZkFold.Algebra.Polynomial.Multivariate.Monomial where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (intercalate)
import Data.Map.Strict (
  Map,
  differenceWith,
  empty,
  filter,
  foldrWithKey,
  fromListWith,
  intersectionWith,
  isSubmapOfBy,
  lookup,
  mapKeys,
  unionWith,
 )
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import GHC.IsList (IsList (..))
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary (..))
import Prelude hiding (Num (..), drop, filter, lcm, length, lookup, sum, take, (!!), (/), (^))

import ZkFold.Algebra.Class

type Variable i = (Eq i, Ord i)

type Monomial i j = (Variable i, Ord j, Semiring j)

-- | Monomial type
newtype Mono i j = M (Map i j)
  deriving (FromJSON, Generic, NFData, ToJSON)

------------------------------------ Map-based monomials ------------------------------------

-- | Monomial constructor
monomial :: Monomial i j => Map i j -> Mono i j
monomial = M . filter (/= zero)

evalMonomial
  :: forall i j b
   . MultiplicativeMonoid b
  => Exponent b j
  => (i -> b) -> Mono i j -> b
evalMonomial f (M m) =
  foldrWithKey (\i j x -> (f i ^ j) * x) (one @b) m

-- | Maps a variable index using the provided `Map`
mapVar :: Variable i => Map i i -> i -> i
mapVar m x = case x `lookup` m of
  Just y -> y
  _ -> error "mapVar: something went wrong"

mapVarMonomial :: Variable i => Map i i -> Mono i j -> Mono i j
mapVarMonomial m (M as) = M $ mapKeys (mapVar m) as

instance Monomial i j => IsList (Mono i j) where
  type Item (Mono i j) = (i, j)
  toList (M m) = toList m
  fromList m = M $ fromListWith (+) m

instance (Show i, Show j, Monomial i j) => Show (Mono i j) where
  show (M m) = intercalate "∙" . map showVar $ toList m
   where
    showVar :: (i, j) -> String
    showVar (i, j) = "x" ++ show i ++ (if j == one then "" else "^" ++ show j)

instance Monomial i j => Eq (Mono i j) where
  M asl == M asr = asl == asr

instance Monomial i j => Ord (Mono i j) where
  compare (M asl) (M asr) = go (toList asl) (toList asr)
   where
    go [] [] = EQ
    go [] _ = LT
    go _ [] = GT
    go ((k1, a1) : xs) ((k2, a2) : ys)
      | k1 == k2 = if a1 == a2 then go xs ys else compare a1 a2
      | otherwise = compare k2 k1

instance (Monomial i j, Arbitrary i, Arbitrary j) => Arbitrary (Mono i j) where
  arbitrary = M <$> arbitrary

instance Monomial i j => MultiplicativeSemigroup (Mono i j) where
  M l * M r = M $ Map.filter (/= zero) $ unionWith (+) l r

instance Monomial i j => Exponent (Mono i j) Natural where
  (^) = natPow

instance Monomial i j => MultiplicativeMonoid (Mono i j) where
  one = M empty

instance (Monomial i j, Ring j) => Exponent (Mono i j) Integer where
  (^) = intPow

instance (Monomial i j, Ring j) => MultiplicativeGroup (Mono i j) where
  invert (M m) = M $ Map.map negate $ m

  M l / M r = M $ differenceWith f l r
   where
    f a b = if a == b then Nothing else Just (a - b)

oneM :: Mono i j -> Bool
oneM (M m) = Map.null m

dividable :: forall i j. Monomial i j => Mono i j -> Mono i j -> Bool
dividable (M l) (M r) = isSubmapOfBy (<=) r l

lcmM :: Monomial i j => Mono i j -> Mono i j -> Mono i j
lcmM (M l) (M r) = M $ unionWith max l r

gcdM :: Monomial i j => Mono i j -> Mono i j -> Mono i j
gcdM (M l) (M r) = M (intersectionWith min l r)
