{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}

module ZkFold.Algebra.Polynomial.Multivariate.Internal where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor ((<&>))
import Data.List (foldl', intercalate)
import Data.Set (Set)
import GHC.Generics (Generic)
import GHC.IsList (IsList (..))
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary (..))
import ZkFold.Algebra.Class
import ZkFold.Algebra.Polynomial.Multivariate.Monomial
import qualified ZkFold.Algebra.Polynomial.Multivariate.Monomial as Mono
import Prelude hiding (
  Num (..),
  drop,
  lcm,
  length,
  sum,
  take,
  (!!),
  (/),
 )

-- | A class for polynomials.
-- `coef` is the coefficient type,
-- `var` is the variable type,
-- `pow` is the power type.
type Polynomial coef var pow = (Eq coef, Field coef, Monomial var pow)

-- | Polynomial type
newtype Poly coef var pow = P [(coef, Mono var pow)]
  deriving (FromJSON, Generic, NFData, ToJSON)

---------------------------------- List-based polynomials with map-based monomials ----------------------------------

-- | Polynomial constructor
polynomial :: Polynomial coef var pow => [(coef, Mono var pow)] -> Poly coef var pow
polynomial = foldr (\(coef, m) x -> if coef == zero then x else P [(coef, m)] + x) zero

evalPolynomial
  :: forall coef var pow a
   . AdditiveMonoid a
  => Scale coef a
  => ((var -> a) -> Mono var pow -> a)
  -> (var -> a)
  -> Poly coef var pow
  -> a
evalPolynomial e f (P p) = foldr (\(coef, m) x -> x + scale coef (e f m)) zero p

variables :: forall coef var pow. Variable var => Poly coef var pow -> Set var
variables (P p) = foldMap (Mono.variables . snd) p

mapVar :: Variable var' => (var -> var') -> Poly coef var pow -> Poly coef var' pow
mapVar f (P ms) = P $ second (Mono.mapVar f) <$> ms

mapCoeffs
  :: forall coef coef' var pow
   . (coef -> coef')
  -> Poly coef var pow
  -> Poly coef' var pow
mapCoeffs f (P p) = P $ p <&> first f

instance Polynomial coef var pow => IsList (Poly coef var pow) where
  type Item (Poly coef var pow) = (coef, Mono var pow)
  toList (P p) = p
  fromList = polynomial

instance (Show coef, Show var, Show pow, Monomial var pow) => Show (Poly coef var pow) where
  show (P p) =
    intercalate " + " $
      p <&> \(coef, m) -> show coef <> "∙" <> show (m :: Mono var pow)

instance Polynomial coef var pow => Eq (Poly coef var pow) where
  P l == P r = l == r

-- TODO: this assumes sorted monomials! Needs fixing.
instance Polynomial coef var pow => Ord (Poly coef var pow) where
  compare (P l) (P r) =
    compare
      (snd <$> l)
      (snd <$> r)

instance (Arbitrary coef, Arbitrary (Mono var pow)) => Arbitrary (Poly coef var pow) where
  arbitrary = P <$> arbitrary

instance {-# OVERLAPPING #-} FromConstant (Poly coef var pow) (Poly coef var pow)

instance Polynomial coef var pow => AdditiveSemigroup (Poly coef var pow) where
  P l + P r = P $ filter ((/= zero) . fst) $ go l r
   where
    go [] [] = []
    go ls [] = ls
    go [] rs = rs
    go ((cl, ml) : ls) ((cr, mr) : rs)
      | cl == zero = go ls ((cr, mr) : rs)
      | cr == zero = go ((cl, ml) : ls) rs
      | ml == mr =
          if cl + cr == zero
            then go ls rs
            else (cl + cr, ml) : go ls rs
      | ml > mr = (cl, ml) : go ls ((cr, mr) : rs)
      | otherwise = (cr, mr) : go ((cl, ml) : ls) rs

instance Scale coef' coef => Scale coef' (Poly coef var pow) where
  scale coef' (P p) = P $ map (first (scale coef')) p

instance Polynomial coef var pow => AdditiveMonoid (Poly coef var pow) where
  zero = P []

instance Polynomial coef var pow => AdditiveGroup (Poly coef var pow) where
  negate (P p) = P $ map (first negate) p

instance {-# OVERLAPPING #-} Polynomial coef var pow => Scale (Poly coef var pow) (Poly coef var pow)

instance Polynomial coef var pow => MultiplicativeSemigroup (Poly coef var pow) where
  P l * r = foldl' (+) (P []) $ map (`scaleM` r) l

instance Polynomial coef var pow => Exponent (Poly coef var pow) Natural where
  (^) = natPow

instance Polynomial coef var pow => MultiplicativeMonoid (Poly coef var pow) where
  one = P [(one, one)]

instance (Monomial var pow, FromConstant coef' coef) => FromConstant coef' (Poly coef var pow) where
  fromConstant x = P [(fromConstant x, one)]

instance Polynomial coef var pow => Semiring (Poly coef var pow)

instance Polynomial coef var pow => Ring (Poly coef var pow)

-- | @'var' x@ is a polynomial \(p(x) = x_{var}\)
var :: Polynomial coef var pow => var -> Poly coef var pow
var x = polynomial [(one, mono $ fromList [(x, one)])]

-- | @'constant' coef@ is a polynomial \(p(x) = const\)
constant :: Polynomial coef var pow => coef -> Poly coef var pow
constant coef = polynomial [(coef, one)]

lt :: Polynomial coef var pow => Poly coef var pow -> (coef, Mono var pow)
lt (P []) = (zero, one)
lt (P (m : _)) = m

zeroP :: Poly coef var pow -> Bool
zeroP (P []) = True
zeroP _ = False

scaleM :: Polynomial coef var pow => (coef, Mono var pow) -> Poly coef var pow -> Poly coef var pow
scaleM (coef, m) (P p) = P $ map (bimap (* coef) (* m)) p
