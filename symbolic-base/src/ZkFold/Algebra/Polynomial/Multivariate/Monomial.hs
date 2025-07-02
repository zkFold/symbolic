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
import ZkFold.Algebra.Class
import Prelude hiding (Num (..), drop, filter, lcm, length, lookup, sum, take, (!!), (/), (^))

-- | A class for variables.
type Variable var = (Eq var, Ord var)

-- | A class for monomials.
type Monomial var pow = (Variable var, Ord pow, Semiring pow)

-- | Monomial type
newtype Mono var pow = UnsafeMono (Map var pow)
  deriving (Generic, NFData, FromJSON, ToJSON)

------------------------------------ Map-based monomials ------------------------------------

-- | Monomial constructor
mono :: Monomial i j => Map i j -> Mono i j
mono = UnsafeMono . filter (/= zero)

evalMonomial
  :: forall i j b
   . MultiplicativeMonoid b
  => Exponent b j
  => (i -> b)
  -> Mono i j
  -> b
evalMonomial f (UnsafeMono m) =
  foldrWithKey (\i j x -> (f i ^ j) * x) (one @b) m

-- | Maps a variable index using the provided `Map`
mapVar :: Variable i => Map i i -> i -> i
mapVar m x = case x `lookup` m of
  Just y -> y
  _ -> error "mapVar: something went wrong"

mapVarMonomial :: Variable i => Map i i -> Mono i j -> Mono i j
mapVarMonomial m (UnsafeMono as) = UnsafeMono $ mapKeys (mapVar m) as

instance Monomial i j => IsList (Mono i j) where
  type Item (Mono i j) = (i, j)
  toList (UnsafeMono m) = toList m
  fromList m = UnsafeMono $ fromListWith (+) m

instance (Show i, Show j, Monomial i j) => Show (Mono i j) where
  show (UnsafeMono m) = intercalate "∙" . map showVar $ toList m
   where
    showVar :: (i, j) -> String
    showVar (i, j) = "x" ++ show i ++ (if j == one then "" else "^" ++ show j)

instance Monomial i j => Eq (Mono i j) where
  UnsafeMono asl == UnsafeMono asr = asl == asr

instance Monomial i j => Ord (Mono i j) where
  compare (UnsafeMono asl) (UnsafeMono asr) = go (toList asl) (toList asr)
   where
    go [] [] = EQ
    go [] _ = LT
    go _ [] = GT
    go ((k1, a1) : xs) ((k2, a2) : ys)
      | k1 == k2 = if a1 == a2 then go xs ys else compare a1 a2
      | otherwise = compare k2 k1

instance (Monomial i j, Arbitrary i, Arbitrary j) => Arbitrary (Mono i j) where
  arbitrary = UnsafeMono <$> arbitrary

instance Monomial i j => MultiplicativeSemigroup (Mono i j) where
  UnsafeMono l * UnsafeMono r = UnsafeMono $ Map.filter (/= zero) $ unionWith (+) l r

instance Monomial i j => Exponent (Mono i j) Natural where
  (^) = natPow

instance Monomial i j => MultiplicativeMonoid (Mono i j) where
  one = UnsafeMono empty

instance (Monomial i j, Ring j) => Exponent (Mono i j) Integer where
  (^) = intPow

instance (Monomial i j, Ring j) => MultiplicativeGroup (Mono i j) where
  invert (UnsafeMono m) = UnsafeMono $ Map.map negate m

  UnsafeMono l / UnsafeMono r = UnsafeMono $ differenceWith f l r
   where
    f a b = if a == b then Nothing else Just (a - b)

oneM :: Mono i j -> Bool
oneM (UnsafeMono m) = Map.null m

dividable :: forall i j. Monomial i j => Mono i j -> Mono i j -> Bool
dividable (UnsafeMono l) (UnsafeMono r) = isSubmapOfBy (<=) r l

lcmM :: Monomial i j => Mono i j -> Mono i j -> Mono i j
lcmM (UnsafeMono l) (UnsafeMono r) = UnsafeMono $ unionWith max l r

gcdM :: Monomial i j => Mono i j -> Mono i j -> Mono i j
gcdM (UnsafeMono l) (UnsafeMono r) = UnsafeMono (intersectionWith min l r)
