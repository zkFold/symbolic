{-# LANGUAGE TypeApplications #-}

module ZkFold.Algebra.Polynomial.Multivariate.Monomial (
  Mono,
  mono,
  evalMonomial,
  variables,
  mapVar,
  degM,
  oneM,
  dividable,
  lcmM,
  gcdM,
) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bool (Bool (..))
import Data.Eq (Eq (..))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (intercalate, map, (++))
import Data.Map.Strict (
  Map,
  differenceWith,
  empty,
  foldrWithKey,
  fromListWith,
  intersectionWith,
  isSubmapOfBy,
  keysSet,
  mapKeys,
  unionWith,
 )
import qualified Data.Map.Strict as Map
import Data.Maybe (Maybe (..))
import Data.Ord (Ord (..), Ordering (..))
import Data.Set (Set)
import GHC.Generics (Generic)
import GHC.IsList (IsList (..))
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary (..))
import ZkFold.Algebra.Class
import Prelude (Integer, Show (..), String, otherwise)

-- | Monomial type
newtype Mono var pow = UnsafeMono (Map var pow)
  deriving (Generic, NFData, FromJSON, ToJSON)

-- | Monomial constructor
mono :: (Eq pow, AdditiveMonoid pow) => Map var pow -> Mono var pow
mono = UnsafeMono . Map.filter (/= zero)

evalMonomial
  :: forall var pow a
   . MultiplicativeMonoid a
  => Exponent a pow
  => (var -> a)
  -> Mono var pow
  -> a
evalMonomial f (UnsafeMono m) =
  foldrWithKey (\var pow x -> (f var ^ pow) * x) (one @a) m

variables :: Mono var pow -> Set var
variables (UnsafeMono m) = keysSet m

mapVar :: Ord var' => (var -> var') -> Mono var pow -> Mono var' pow
mapVar f (UnsafeMono as) = UnsafeMono $ mapKeys f as

instance (Ord var, AdditiveSemigroup pow) => IsList (Mono var pow) where
  type Item (Mono var pow) = (var, pow)
  toList (UnsafeMono m) = Map.toList m
  fromList m = UnsafeMono $ fromListWith (+) m

instance (Show var, Show pow, Eq pow, MultiplicativeMonoid pow) => Show (Mono var pow) where
  show (UnsafeMono m) = intercalate "∙" . map showVar $ Map.toList m
   where
    showVar :: (var, pow) -> String
    showVar (v, p) = "x" ++ show v ++ (if p == one then "" else "^" ++ show p)

instance (Eq var, Eq pow) => Eq (Mono var pow) where
  UnsafeMono asl == UnsafeMono asr = asl == asr

instance (Ord var, Ord pow) => Ord (Mono var pow) where
  compare (UnsafeMono asl) (UnsafeMono asr) = go (Map.toList asl) (Map.toList asr)
   where
    go [] [] = EQ
    go [] _ = LT
    go _ [] = GT
    go ((k1, a1) : xs) ((k2, a2) : ys)
      | k1 == k2 = if a1 == a2 then go xs ys else compare a1 a2
      | otherwise = compare k2 k1

instance (Ord var, Arbitrary var, Arbitrary pow) => Arbitrary (Mono var pow) where
  arbitrary = UnsafeMono <$> arbitrary

instance (Ord var, Eq pow, AdditiveMonoid pow) => MultiplicativeSemigroup (Mono var pow) where
  UnsafeMono l * UnsafeMono r = UnsafeMono $ Map.filter (/= zero) $ unionWith (+) l r

instance (Ord var, Eq pow, AdditiveMonoid pow) => Exponent (Mono var pow) Natural where
  (^) = natPow

instance (Ord var, Eq pow, AdditiveMonoid pow) => MultiplicativeMonoid (Mono var pow) where
  one = UnsafeMono empty

instance (Ord var, Eq pow, Ring pow) => Exponent (Mono var pow) Integer where
  (^) = intPow

instance (Ord var, Eq pow, Ring pow) => MultiplicativeGroup (Mono var pow) where
  invert (UnsafeMono m) = UnsafeMono $ Map.map negate m

  UnsafeMono l / UnsafeMono r = UnsafeMono $ differenceWith f l r
   where
    f a b = if a == b then Nothing else Just (a - b)

degM :: AdditiveMonoid pow => Mono var pow -> pow
degM (UnsafeMono m) = sum m

oneM :: Mono var pow -> Bool
oneM (UnsafeMono m) = Map.null m

dividable :: (Ord var, Ord pow) => Mono var pow -> Mono var pow -> Bool
dividable (UnsafeMono l) (UnsafeMono r) = isSubmapOfBy (<=) r l

lcmM :: (Ord var, Ord pow) => Mono var pow -> Mono var pow -> Mono var pow
lcmM (UnsafeMono l) (UnsafeMono r) = UnsafeMono $ unionWith max l r

gcdM :: (Ord var, Ord pow) => Mono var pow -> Mono var pow -> Mono var pow
gcdM (UnsafeMono l) (UnsafeMono r) = UnsafeMono (intersectionWith min l r)
