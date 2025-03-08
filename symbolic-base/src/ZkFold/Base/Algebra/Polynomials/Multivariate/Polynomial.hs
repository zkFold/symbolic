{-# LANGUAGE DeriveAnyClass               #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}

module ZkFold.Base.Algebra.Polynomials.Multivariate.Polynomial where

import           Control.DeepSeq                                       (NFData)
import           Data.Aeson                                            (FromJSON, ToJSON)
import           Data.Bifunctor                                        (Bifunctor (..))
import           Data.Functor                                          ((<&>))
import           Data.List                                             (intercalate)
import           Data.Map.Strict                                       (Map, empty, keysSet)
import qualified Data.Map.Strict                                       as M
import           Data.Set                                              (Set)
import           GHC.Generics                                          (Generic)
import           GHC.IsList                                            (IsList (..))
import           Numeric.Natural                                       (Natural)
import           Prelude                                               hiding (Num (..), drop, lcm, length, sum, take,
                                                                        (!!), (/))
import           Test.QuickCheck                                       (Arbitrary (..))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Polynomials.Multivariate.Monomial

-- | A class for polynomials.
-- `c` is the coefficient type,
-- `i` is the variable type,
-- `j` is the power type.
type Polynomial c i j = (Eq c, Field c, Monomial i j)

-- | Polynomial type
newtype Poly c i j = P [(c, Mono i j)]
    deriving (Generic, NFData, FromJSON, ToJSON)

---------------------------------- List-based polynomials with map-based monomials ----------------------------------

-- | Polynomial constructor
polynomial :: Polynomial c i j => [(c, Mono i j)] -> Poly c i j
polynomial = foldr (\(c, m) x -> if c == zero then x else P [(c, m)] + x) zero

evalPolynomial
    :: forall c i j b
     . AdditiveMonoid b
    => Scale c b
    => ((i -> b) -> Mono i j -> b)
    -> (i -> b)
    -> Poly c i j
    -> b
evalPolynomial e f (P p) = foldr (\(c, m) x -> x + scale c (e f m)) zero p

variables :: forall c v . Ord v => Poly c v Natural -> Set v
variables (P p) = foldMap ((\(M m) -> keysSet m) . snd) p

mapVars :: Variable i2 => (i1 -> i2) -> Poly c i1 j -> Poly c i2 j
mapVars f (P ms) = P $ (\(c, M m) -> (c, M $ M.mapKeys f m)) <$> ms

mapVarPolynomial :: Variable i => Map i i-> Poly c i j -> Poly c i j
mapVarPolynomial m (P ms) = P $ second (mapVarMonomial m) <$> ms

mapCoeffs :: forall c c' i j .
    (c -> c')
    -> Poly c i j
    -> Poly c' i j
mapCoeffs f (P p) = P $ p <&> first f

instance Polynomial c i j => IsList (Poly c i j) where
    type Item (Poly c i j) = (c, Map i j)
    toList (P p) = second (\(M m) -> m) <$> p
    fromList p = polynomial $ second monomial <$> p

instance (Show c, Show i, Show j, Monomial i j) => Show (Poly c i j) where
    show (P p) = intercalate " + "
        $ p <&> \(c, m) -> show c <> "∙" <> show (m :: Mono i j)

instance Polynomial c i j => Eq (Poly c i j) where
    P l == P r = l == r

-- TODO: this assumes sorted monomials! Needs fixing.
instance Polynomial c i j => Ord (Poly c i j) where
    compare (P l) (P r) = compare
        (snd <$> l)
        (snd <$> r)

instance (Arbitrary c, Arbitrary (Mono i j)) => Arbitrary (Poly c i j) where
    arbitrary = P <$> arbitrary

instance {-# OVERLAPPING #-} FromConstant (Poly c i j) (Poly c i j)

instance Polynomial c i j => AdditiveSemigroup (Poly c i j) where
    P l + P r = P $ filter ((/= zero) . fst) $ go l r
        where
            go [] [] = []
            go ls [] = ls
            go [] rs = rs
            go ((cl, ml):ls) ((cr, mr):rs)
                | cl == zero = go ls ((cr, mr):rs)
                | cr == zero = go ((cl, ml):ls) rs
                | ml == mr =
                    if cl + cr == zero
                        then go ls rs
                        else (cl + cr, ml) : go ls rs
                | ml > mr   = (cl, ml) : go ls ((cr, mr):rs)
                | otherwise = (cr, mr) : go ((cl, ml):ls) rs

instance Scale c' c => Scale c' (Poly c i j) where
    scale c' (P p) = P $ map (first (scale c')) p

instance Polynomial c i j => AdditiveMonoid (Poly c i j) where
    zero = P []

instance Polynomial c i j => AdditiveGroup (Poly c i j) where
    negate (P p) = P $ map (first negate) p

instance {-# OVERLAPPING #-} Polynomial c i j => Scale (Poly c i j) (Poly c i j)

instance Polynomial c i j => MultiplicativeSemigroup (Poly c i j) where
    P l * r = foldl' (+) (P []) $ map (`scaleM` r) l

instance Polynomial c i j => Exponent (Poly c i j) Natural where
    (^) = natPow

instance Polynomial c i j => MultiplicativeMonoid (Poly c i j) where
    one = P [(one, M empty)]

instance FromConstant c' c => FromConstant c' (Poly c i j) where
    fromConstant x = P [(fromConstant x, M empty)]

instance Polynomial c i j => Semiring (Poly c i j)

instance Polynomial c i j => Ring (Poly c i j)

-- | @'var' i@ is a polynomial \(p(x) = x_i\)
var :: Polynomial c i j => i -> Poly c i j
var x = polynomial [(one, monomial $ fromList [(x, one)])]

-- | @'constant' i@ is a polynomial \(p(x) = const\)
constant :: Polynomial c i j => c -> Poly c i j
constant c = polynomial [(c, M M.empty)]

lt :: Polynomial c i j => Poly c i j -> (c, Mono i j)
lt (P [])    = (zero, M empty)
lt (P (m:_)) = m

zeroP :: Poly c i j -> Bool
zeroP (P []) = True
zeroP _      = False

scaleM :: Polynomial c i j => (c, Mono i j) -> Poly c i j -> Poly c i j
scaleM (c, m) (P p) = P $ map (bimap (* c) (* m)) p
