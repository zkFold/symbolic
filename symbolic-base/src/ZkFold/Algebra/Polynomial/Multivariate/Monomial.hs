{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}

module ZkFold.Algebra.Polynomial.Multivariate.Monomial where

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
  lookup,
  mapKeys,
  unionWith,
 )
import qualified Data.Map.Strict as Map
import Data.Maybe (Maybe (..))
import Data.Ord (Ord (..), Ordering (..))
import GHC.Generics (Generic)
import GHC.IsList (IsList (..))
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary (..))
import ZkFold.Algebra.Class
import Prelude (Integer, Show (..), String, error, otherwise)

-- import Prelude hiding (Num (..), drop, filter, lcm, length, lookup, sum, take, (!!), (/), (^))

-- | A class for variables.
type Variable var = (Ord var)

-- | A class for monomials.
type Monomial var pow = (Variable var, Ord pow, Semiring pow)

-- | Monomial type
newtype Mono var pow = UnsafeMono (Map var pow)
  deriving (Generic, NFData, FromJSON, ToJSON)

------------------------------------ Map-based monomials ------------------------------------

-- | Monomial constructor
mono :: Monomial var pow => Map var pow -> Mono var pow
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

-- | Maps a variable index using the provided `Map`
mapVar :: Variable var => Map var var -> var -> var
mapVar m x = case x `lookup` m of
  Just y -> y
  _ -> error "mapVar: something went wrong"

mapVarMonomial :: Variable var => Map var var -> Mono var pow -> Mono var pow
mapVarMonomial m (UnsafeMono as) = UnsafeMono $ mapKeys (mapVar m) as

instance Monomial var pow => IsList (Mono var pow) where
  type Item (Mono var pow) = (var, pow)
  toList (UnsafeMono m) = toList m
  fromList m = UnsafeMono $ fromListWith (+) m

instance (Show var, Show pow, Monomial var pow) => Show (Mono var pow) where
  show (UnsafeMono m) = intercalate "∙" . map showVar $ toList m
   where
    showVar :: (var, pow) -> String
    showVar (v, p) = "x" ++ show v ++ (if p == one then "" else "^" ++ show p)

instance Monomial var pow => Eq (Mono var pow) where
  UnsafeMono asl == UnsafeMono asr = asl == asr

instance Monomial var pow => Ord (Mono var pow) where
  compare (UnsafeMono asl) (UnsafeMono asr) = go (toList asl) (toList asr)
   where
    go [] [] = EQ
    go [] _ = LT
    go _ [] = GT
    go ((k1, a1) : xs) ((k2, a2) : ys)
      | k1 == k2 = if a1 == a2 then go xs ys else compare a1 a2
      | otherwise = compare k2 k1

instance (Monomial var pow, Arbitrary var, Arbitrary pow) => Arbitrary (Mono var pow) where
  arbitrary = UnsafeMono <$> arbitrary

instance Monomial var pow => MultiplicativeSemigroup (Mono var pow) where
  UnsafeMono l * UnsafeMono r = UnsafeMono $ Map.filter (/= zero) $ unionWith (+) l r

instance Monomial var pow => Exponent (Mono var pow) Natural where
  (^) = natPow

instance Monomial var pow => MultiplicativeMonoid (Mono var pow) where
  one = UnsafeMono empty

instance (Monomial var pow, Ring pow) => Exponent (Mono var pow) Integer where
  (^) = intPow

instance (Monomial var pow, Ring pow) => MultiplicativeGroup (Mono var pow) where
  invert (UnsafeMono m) = UnsafeMono $ Map.map negate m

  UnsafeMono l / UnsafeMono r = UnsafeMono $ differenceWith f l r
   where
    f a b = if a == b then Nothing else Just (a - b)

oneM :: Mono var pow -> Bool
oneM (UnsafeMono m) = Map.null m

dividable :: forall var pow. Monomial var pow => Mono var pow -> Mono var pow -> Bool
dividable (UnsafeMono l) (UnsafeMono r) = isSubmapOfBy (<=) r l

lcmM :: Monomial var pow => Mono var pow -> Mono var pow -> Mono var pow
lcmM (UnsafeMono l) (UnsafeMono r) = UnsafeMono $ unionWith max l r

gcdM :: Monomial var pow => Mono var pow -> Mono var pow -> Mono var pow
gcdM (UnsafeMono l) (UnsafeMono r) = UnsafeMono (intersectionWith min l r)
