{-# LANGUAGE AllowAmbiguousTypes          #-}
{-# LANGUAGE DeriveAnyClass               #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints        #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE UndecidableInstances         #-}


{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Base.Algebra.Polynomials.Univariate
    ( Poly
    , removeZeros
    , PolyVec
    , rewrapPolyVec
    , mulVector
    , mulDft
    , mulKaratsuba
    , mulPoly
    , mulPolyKaratsuba
    , mulPolyDft
    , mulPolyNaive
    , UnivariateRingPolynomial(..)
    , UnivariateFieldPolynomial(..)
    , UnivariateRingPolyVec(..)
    , UnivariateFieldPolyVec(..)
    ) where

import           Control.DeepSeq                  (NFData (..))
import qualified Data.Vector                      as V
import           GHC.Generics                     (Generic)
import           GHC.IsList                       (IsList (..))
import           Prelude                          hiding (Num (..), drop, length, product, replicate, sum, take, (/),
                                                   (^))
import qualified Prelude                          as P
import           Test.QuickCheck                  (Arbitrary (..), chooseInt)

import           ZkFold.Base.Algebra.Basic.Class  hiding (Euclidean (..))
import           ZkFold.Base.Algebra.Basic.DFT    (genericDft)
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Prelude                   (replicate, zipWithDefault)

infixl 7 .*, *., .*., ./.
infixl 6 .+, +.

-------------------------------- Arbitrary degree polynomials --------------------------------

class
    ( Ring c
    , AdditiveGroup poly
    ) => UnivariateRingPolynomial c poly | poly -> c where

    constant :: c -> poly

    fromPoly :: poly -> V.Vector c

    toPoly :: V.Vector c -> poly

    -- | Leading coefficient
    lt :: poly -> c

    -- | Degree of zero polynomial is `-1`
    deg :: poly -> Integer

    -- | Return `c * x ^ n`
    monomial
        :: Natural -- ^ n
        -> c -- ^ c
        -> poly

    -- | Evaluate polynomial
    evalPoly :: poly -> c -> c

    -- | Return `c * p * (x ^ n)`
    scaleP
        :: c -- ^ c
        -> Natural -- ^ n
        -> poly -- ^ p
        -> poly

class
    ( Field c
    , Eq c
    , MultiplicativeMonoid poly
    , UnivariateRingPolynomial c poly
    ) => UnivariateFieldPolynomial c poly | poly -> c where

    qr :: poly -> poly -> (poly, poly)

    -- | Extended Euclidean algorithm.
    eea :: poly -> poly -> (poly, poly)

instance
    ( Ring c
    , Eq c
    ) => UnivariateRingPolynomial c (Poly c) where

    constant = P . V.singleton

    fromPoly (P cs) = cs

    lt (P cs) = V.last cs

    deg (P cs) = fromIntegral (V.length cs) - 1

    toPoly = removeZeros . P

    monomial d c = P $ V.fromList (replicate d zero P.<> [c])

    evalPoly (P cs) x = sum $ V.zipWith (*) cs $ fmap (x^) (V.generate (V.length cs) (fromIntegral @_ @Natural))

    scaleP a n (P cs) = P $ V.replicate (fromIntegral n) zero V.++ fmap (a *) cs

instance
    ( Field c
    , Eq c
    ) => UnivariateFieldPolynomial c (Poly c) where

    qr a b = go a b zero
        where
            go x y q = if deg x < deg y then (q, x) else go x' y q'
                where
                    c = lt x // lt y
                    n = fromIntegral (deg x - deg y)
                    -- ^ if `deg x < deg y`, `n` is not evaluated, so this would not error out
                    x' = x - scaleP c n y
                    q' = q + scaleP c n one

    eea a b = go (a, one) (b, zero)
        where
            go (x, s) (y, t) = if deg y == -1 then (x, s) else go (y, t) (r, s - q * t)
                where
                    (q, r) = qr x y


-- TODO (Issue #17): hide constructor
newtype Poly c = P (V.Vector c)
    deriving (Eq, Show, Functor, Generic, NFData)

instance {-# OVERLAPPING #-} FromConstant (Poly c) (Poly c)

instance FromConstant c c' => FromConstant c (Poly c') where
    fromConstant = P . V.singleton . fromConstant

instance (Ring c, Eq c) => AdditiveSemigroup (Poly c) where
    P l + P r = removeZeros $ P $ V.zipWith (+) lPadded rPadded
      where
        len = max (V.length l) (V.length r)

        lPadded = l V.++ V.replicate (len P.- V.length l) zero
        rPadded = r V.++ V.replicate (len P.- V.length r) zero

instance {-# OVERLAPPING #-} (Field c, Eq c) => Scale (Poly c) (Poly c)

instance Scale k c => Scale k (Poly c) where
    scale = fmap . scale

instance (Ring c, Eq c) => AdditiveMonoid (Poly c) where
    zero = P V.empty

instance (Ring c, Eq c) => AdditiveGroup (Poly c) where
    negate (P cs) = P $ fmap negate cs

instance (Field c, Eq c) => MultiplicativeSemigroup (Poly c) where
    -- | If it is possible to calculate a primitive root of unity in the field, proceed with FFT multiplication.
    -- Otherwise default to Karatsuba multiplication for polynomials of degree higher than 64 or use naive multiplication otherwise.
    -- 64 is a threshold determined by benchmarking.
    P l * P r = removeZeros $ P $ mulAdaptive l r

padVector :: forall a . Ring a => V.Vector a -> Int -> V.Vector a
padVector v l
  | V.length v == l = v
  | otherwise = v V.++ V.replicate (l P.- V.length v) zero

mulAdaptive :: forall c . Field c => V.Vector c -> V.Vector c -> V.Vector c
mulAdaptive l r
      | V.null l = V.empty
      | V.null r = V.empty
      | otherwise =
          case (maybeW2n, len <= 64) of
            (_, True)        -> mulVector l r
            (Just w2n, _)    -> mulDft (p + 1) w2n lPaddedDft rPaddedDft
            (Nothing, False) -> mulKaratsuba lPaddedKaratsuba rPaddedKaratsuba
        where
            len :: Int
            len = max (V.length l) (V.length r)

            p :: Integer
            p = ceiling @Double $ logBase 2 (fromIntegral len)

            padKaratsuba :: Int
            padKaratsuba = 2 P.^ p

            padDft :: Int
            padDft = 2 P.* padKaratsuba

            lPaddedKaratsuba, rPaddedKaratsuba :: V.Vector c
            lPaddedKaratsuba = padVector l padKaratsuba
            rPaddedKaratsuba = padVector r padKaratsuba

            lPaddedDft, rPaddedDft :: V.Vector c
            lPaddedDft = padVector l padDft
            rPaddedDft = padVector r padDft

            maybeW2n :: Maybe c
            maybeW2n = rootOfUnity $ fromIntegral (p P.+ 1)

mulDft :: forall c . Field c => Integer -> c -> V.Vector c -> V.Vector c -> V.Vector c
mulDft p w2n lPadded rPadded = c
  where
    pad :: Int
    pad = 2 P.^ p

    w2nInv :: c
    w2nInv = one // w2n

    nInv :: c
    nInv = one // fromConstant (fromIntegral @_ @Natural pad)

    v1Image, v2Image :: V.Vector c
    v1Image = genericDft p w2n lPadded
    v2Image = genericDft p w2n rPadded

    cImage :: V.Vector c
    cImage = V.zipWith (*) v1Image v2Image

    c :: V.Vector c
    c = (* nInv) <$> genericDft p w2nInv cImage

mulKaratsuba :: forall a. Field a => V.Vector a -> V.Vector a -> V.Vector a
mulKaratsuba v1 v2
  | len == 1 = V.zipWith (*) v1 v2
  | otherwise = result
  where
    len :: Int
    len = V.length v1

    n :: Int
    n = len `P.div` 2

    a, b, c, d :: V.Vector a
    (b, a) = V.splitAt n v1

    (d, c) = V.splitAt n v2

    partLen :: Int
    partLen = len P.- 1


    ac, bd :: V.Vector a
    ac = padVector (mulAdaptive a c) partLen
    bd = padVector (mulAdaptive b d) partLen

    apb, cpd :: V.Vector a
    apb = V.zipWith (+) a b
    cpd = V.zipWith (+) c d

    abcd :: V.Vector a
    abcd = mulAdaptive apb cpd

    mid :: V.Vector a
    mid = V.zipWith3 (\x y z -> x - y - z) (padVector abcd partLen) (padVector ac partLen) (padVector bd partLen)

    result :: V.Vector a
    result = V.generate (2 P.* len P.- 1) ix2v

    ix2v :: Int -> a
    ix2v ix
      | ix < n = bd `V.unsafeIndex` ix
      | ix < 2 P.* n P.- 1 = bd `V.unsafeIndex` ix + mid `V.unsafeIndex` (ix P.- n)
      | ix == 2 P.* n P.- 1 = mid `V.unsafeIndex` (n P.- 1)
      | ix < 3 P.* n P.- 1 = mid `V.unsafeIndex` (ix P.- n) + ac `V.unsafeIndex` (ix P.- 2 P.* n)
      | otherwise = ac `V.unsafeIndex` (ix P.- 2 P.* n)

mulVector :: forall a. Field a => V.Vector a -> V.Vector a -> V.Vector a
mulVector v1 v2 = result
  where
    len1 = V.length v1
    len2 = V.length v2

    result = V.generate (len1 P.+ len2 P.- 1) ix2v

    ix2v :: Int -> a
    ix2v ix = ix2v' start1 start2 zero
      where
        start1 = min ix (len1 P.- 1)
        start2 = max 0 (ix P.- len1 P.+ 1)

    ix2v' :: Int -> Int -> a -> a
    ix2v' (-1) _ accum                = accum
    ix2v' _ ((== len2) -> True) accum = accum
    ix2v' i j accum                   = ix2v' (i P.- 1) (j P.+ 1) (accum + v1 `V.unsafeIndex` i * v2 `V.unsafeIndex` j)

instance (Field c, Eq c) => Exponent (Poly c) Natural where
    (^) = natPow

instance (Field c, Eq c) => MultiplicativeMonoid (Poly c) where
    one = P $ V.singleton one

instance (Ring c, Arbitrary c, Eq c) => Arbitrary (Poly c) where
    arbitrary = fmap toPoly $ chooseInt (0, 128) >>= \n -> V.replicateM n arbitrary

---------------------------------- Fixed degree polynomials ----------------------------------

newtype PolyVec c (size :: Natural) = PV (V.Vector c)
    deriving (Eq, Show, Generic, NFData)

class
    ( Ring c
    , C pv ~ c
    , forall size . (KnownNat size) => AdditiveGroup (pv size)
    ) => UnivariateRingPolyVec c pv | pv -> c where

    type C pv

-- -- | Multiply the corresponding coefficients of two polynomials.
    (.*.) :: forall size . (KnownNat size) => pv size -> pv size -> pv size

-- -- | Multiply every coefficient of the polynomial by a constant.
    (.*) :: forall size . (KnownNat size) => pv size -> c -> pv size

-- -- | Multiply every coefficient of the polynomial by a constant.
    (*.) :: forall size . (KnownNat size) => c -> pv size -> pv size

-- -- | Add a constant to every coefficient of the polynomial.
    (.+) :: forall size . (KnownNat size) => pv size -> c -> pv size

-- -- | Add a constant to every coefficient of the polynomial.
    (+.) :: forall size . (KnownNat size) => c -> pv size -> pv size

    toPolyVec :: forall size . (KnownNat size) => V.Vector c -> pv size

    fromPolyVec :: forall size . (KnownNat size) => pv size -> V.Vector c

    poly2vec :: forall poly size . (KnownNat size, UnivariateRingPolynomial c poly) => poly -> pv size

    vec2poly :: forall poly size . (KnownNat size, UnivariateRingPolynomial c poly) => pv size -> poly

    -- -- p(x) = a0
    polyVecConstant :: forall size . (KnownNat size) => c -> pv size

    -- p(x) = a1 * x + a0
    polyVecLinear
        :: forall size . (KnownNat size)
        => c -- ^ a1
        -> c -- ^ a0
        -> pv size

    -- p(x) = a2 * x^2 + a1 * x + a0
    polyVecQuadratic
        :: forall size . (KnownNat size)
        => c -- ^ a2
        -> c -- ^ a1
        -> c -- ^ a0
        -> pv size

    evalPolyVec :: forall size . (KnownNat size) => pv size -> c -> c

class
    ( Field c
    , forall size . (KnownNat size) => Ring (pv size)
    , UnivariateRingPolyVec c pv
    ) => UnivariateFieldPolyVec c pv | pv -> c where

-- -- | Divide the corresponding coefficients of two polynomials.
    (./.) :: forall size . (KnownNat size) => pv size -> pv size -> pv size

-- -- p(x) = x^n - 1
    polyVecZero :: forall size . (KnownNat size) => Natural -> pv size

-- -- L_i(x) : p(omega^i) = 1, p(omega^j) = 0, j /= i, 1 <= i <= n, 1 <= j <= n
    polyVecLagrange :: forall size . (KnownNat size) => Natural -> Natural -> c -> pv size

-- -- p(x) = c_1 * L_1(x) + c_2 * L_2(x) + ... + c_n * L_n(x)
    polyVecInLagrangeBasis :: forall n size . (KnownNat size, KnownNat n) => c -> pv n -> pv size

    polyVecGrandProduct :: forall size . (KnownNat size) => pv size -> pv size -> pv size -> c -> c -> pv size

    polyVecDiv :: forall size . (KnownNat size) => pv size -> pv size -> pv size

    castPolyVec :: forall size size' . (KnownNat size, KnownNat size') => pv size -> pv size'

instance
    ( Ring c
    ) => UnivariateRingPolyVec c (PolyVec c) where

    type C (PolyVec c) = c

    l .*. r = toPolyVec @_ @(PolyVec c) $ fromList $ zipWith (*) (toList $ fromPolyVec l) (toList $ fromPolyVec r)

    (PV cs) .* a = PV $ fmap (* a) cs

    a *. (PV cs) = PV $ fmap (a *) cs

    (PV cs) .+ a = PV $ fmap (+ a) cs

    a +. (PV cs) = PV $ fmap (+ a) cs

    toPolyVec :: forall size . (KnownNat size) => V.Vector c -> PolyVec c size
    toPolyVec = PV . V.take (fromIntegral (value @size)) . addZeros @c @size

    fromPolyVec :: forall size . PolyVec c size -> V.Vector c
    fromPolyVec (PV cs) = cs

    poly2vec :: forall poly size . (KnownNat size, UnivariateRingPolynomial c poly) => poly -> PolyVec c size
    poly2vec = toPolyVec . fromPoly

    vec2poly :: forall poly size . (KnownNat size, UnivariateRingPolynomial c poly) => PolyVec c size -> poly
    vec2poly = toPoly . fromPolyVec

    -- -- p(x) = a0
    polyVecConstant :: forall size . (KnownNat size) => c -> PolyVec c size
    polyVecConstant a0 = PV $ V.singleton a0 V.++ V.replicate (fromIntegral $ value @size -! 1) zero

    -- p(x) = a1 * x + a0
    polyVecLinear :: forall size . (KnownNat size) => c -> c -> PolyVec c size
    polyVecLinear a1 a0 = PV $ V.fromList [a0, a1] V.++ V.replicate (fromIntegral $ value @size -! 2) zero

    -- p(x) = a2 * x^2 + a1 * x + a0
    polyVecQuadratic :: forall size . (KnownNat size) => c -> c -> c -> PolyVec c size
    polyVecQuadratic a2 a1 a0  = PV $ V.fromList [a0, a1, a2] V.++ V.replicate (fromIntegral $ value @size -! 3) zero

    evalPolyVec :: forall size . PolyVec c size -> c -> c
    evalPolyVec (PV cs) x = sum $ V.zipWith (*) cs $ fmap (x^) (V.generate (V.length cs) (fromIntegral @_ @Natural))

instance
    ( Field c
    , Eq c
    , UnivariateRingPolyVec c (PolyVec c)
    ) => UnivariateFieldPolyVec c (PolyVec c) where

    l ./. r = toPolyVec $ fromList $ zipWith (//) (toList $ fromPolyVec l) (toList $ fromPolyVec r)

    polyVecZero :: forall size . (KnownNat size) => Natural -> PolyVec c size
    polyVecZero n = poly2vec $ scaleP one n (one @(Poly c)) - one @(Poly c)

    polyVecLagrange :: forall size . (KnownNat size) => Natural -> Natural -> c -> PolyVec c size
    polyVecLagrange n i omega = (*.) (omega^i // fromConstant n) $ (polyVecZero n - one) `polyVecDiv` polyVecLinear one (negate $ omega^i)

    polyVecInLagrangeBasis :: forall n size . (KnownNat n, KnownNat size) => c -> PolyVec c n -> PolyVec c size
    polyVecInLagrangeBasis omega (PV cs) =
        let ls = fmap (\i -> polyVecLagrange (value @n) i omega) (V.generate (V.length cs) (fromIntegral . succ))
        in sum $ V.zipWith (*.) cs ls

    polyVecGrandProduct :: forall size . (KnownNat size) => PolyVec c size -> PolyVec c size -> PolyVec c size -> c -> c -> PolyVec c size
    polyVecGrandProduct (PV as) (PV bs) (PV sigmas) beta gamma =
        let ps = fmap (+ gamma) (V.zipWith (+) as (fmap (* beta) bs))
            qs = fmap (+ gamma) (V.zipWith (+) as (fmap (* beta) sigmas))
            zs = fmap (product . flip V.take (V.zipWith (//) ps qs)) (V.generate (fromIntegral (value @size)) id)
        in PV zs

    polyVecDiv :: forall size . (KnownNat size) => PolyVec c size -> PolyVec c size -> PolyVec c size
    polyVecDiv l r = poly2vec $ fst $ qr @c @(Poly c) (vec2poly l) (vec2poly r)

    castPolyVec :: forall size size' . (KnownNat size, KnownNat size') => PolyVec c size -> PolyVec c size'
    castPolyVec (PV cs)
        | value @size <= value @size'                             = toPolyVec cs
        | all (== zero) (V.drop (fromIntegral (value @size')) cs) = toPolyVec cs
        | otherwise = error "castPolyVec: Cannot cast polynomial vector to smaller size!"

instance
    ( UnivariateRingPolyVec c pv
    , KnownNat size
    ) => IsList (pv size) where
    type Item (pv size) = C pv
    fromList = toPolyVec . V.fromList
    toList = V.toList . fromPolyVec

instance Scale c' c => Scale c' (PolyVec c size) where
    scale c (PV p) = PV (scale c <$> p)

instance (FromConstant Natural c, AdditiveMonoid c, KnownNat size) => FromConstant Natural (PolyVec c size) where
    fromConstant n = PV $ V.singleton (fromConstant n) V.++ V.replicate (fromIntegral (value @size -! 1)) zero

instance (FromConstant Integer c, AdditiveMonoid c, KnownNat size) => FromConstant Integer (PolyVec c size) where
    fromConstant n = PV $ V.singleton (fromConstant n) V.++ V.replicate (fromIntegral (value @size -! 1)) zero

instance Ring c => AdditiveSemigroup (PolyVec c size) where
    PV l + PV r = PV $ V.zipWith (+) l r

instance (Ring c, KnownNat size) => AdditiveMonoid (PolyVec c size) where
    zero = PV $ V.replicate (fromIntegral (value @size)) zero

instance (Ring c, KnownNat size) => AdditiveGroup (PolyVec c size) where
    negate (PV cs) = PV $ fmap negate cs

instance (Field c, KnownNat size) => Exponent (PolyVec c size) Natural where
    (^) = natPow

instance {-# OVERLAPPING #-} (Field c, KnownNat size) => Scale (PolyVec c size) (PolyVec c size)

-- TODO (Issue #18): check for overflow
instance (Field c, KnownNat size) => MultiplicativeSemigroup (PolyVec c size) where
    (PV l) * (PV r) = toPolyVec $ mulAdaptive l r

instance (Field c, KnownNat size) => MultiplicativeMonoid (PolyVec c size) where
    one = PV $ V.singleton one V.++ V.replicate (fromIntegral (value @size -! 1)) zero

instance (Field c, KnownNat size) => Semiring (PolyVec c size)

instance (Field c, KnownNat size) => Ring (PolyVec c size)

instance (Ring c, Arbitrary c, KnownNat size) => Arbitrary (PolyVec c size) where
    arbitrary = toPolyVec @_ @(PolyVec c) <$> V.replicateM (fromIntegral $ value @size) (arbitrary @c)

-------------------------------- Helper functions --------------------------------

rewrapPolyVec :: (V.Vector c -> V.Vector c) -> PolyVec c size -> PolyVec c size
rewrapPolyVec f (PV x) = PV (f x)

removeZeros :: (Ring c, Eq c) => Poly c -> Poly c
removeZeros (P cs)
  | V.null cs = P cs
  | otherwise = P $ V.take (1 P.+ traverseZeros startIx) cs
    where
        startIx :: Int
        startIx = V.length cs P.- 1

        traverseZeros :: Int -> Int
        traverseZeros 0
          | V.head cs == zero = -1
          | otherwise = 0
        traverseZeros n
          | cs `V.unsafeIndex` n == zero = traverseZeros (n P.- 1)
          | otherwise = n

addZeros :: forall c size . (Ring c, KnownNat size) => V.Vector c -> V.Vector c
addZeros cs = cs V.++ V.replicate (fromIntegral (value @size) P.- V.length cs) zero


-- ** THE CODE BELOW IS ONLY USED FOR BENCHMARKING MULTIPLICATION **

-- | Naive vector multiplication, O(n^2)
--
mulPoly :: forall a. Field a => Poly a -> Poly a -> Poly a
mulPoly (P v1) (P v2) = P $ mulVector v1 v2

-- | Adaptation of Karatsuba's algorithm. O(n^log_2(3))
--
mulPolyKaratsuba :: (Eq a, Field a) => Poly a -> Poly a -> Poly a
mulPolyKaratsuba (P v1) (P v2) = removeZeros $ P result
  where
    l = max (V.length v1) (V.length v2)
    p = ceiling @Double @Integer $ logBase 2 (fromIntegral l)

    pad = 2 P.^ p

    result = mulKaratsuba
        (v1 V.++ V.replicate (pad P.- V.length v1) zero)
        (v2 V.++ V.replicate (pad P.- V.length v2) zero)

-- DFT multiplication of vectors. O(nlogn)
--
mulPolyDft :: forall a . (Eq a, Field a) => Poly a -> Poly a -> Poly a
mulPolyDft (P v1) (P v2) = removeZeros $ P result
  where
    l = max (V.length v1) (V.length v2)
    p = (ceiling @Double $ logBase 2 (fromIntegral l)) P.+ 1

    w2n :: a
    w2n = case rootOfUnity $ fromIntegral p of
            Just a -> a
            _      -> undefined

    pad = 2 P.^ p

    result = mulDft @a p w2n
        (v1 V.++ V.replicate (pad P.- V.length v1) zero)
        (v2 V.++ V.replicate (pad P.- V.length v2) zero)

mulPolyNaive :: Field a => Poly a -> Poly a -> Poly a
mulPolyNaive (P v1) (P v2) = P $ V.fromList $ go (V.toList v1) (V.toList v2)
    where
        go [] _      = []
        go (x:xs) ys = zipWithDefault (+) zero zero (map (x *) ys) (zero : go xs ys)
