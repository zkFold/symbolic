{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Algebra.Polynomial.Univariate (
  Poly,
  removeZeros,
  PolyVec (..),
  rewrapPolyVec,
  mulVector,
  mulDft,
  mulKaratsuba,
  mulPoly,
  mulPolyKaratsuba,
  mulPolyDft,
  mulPolyNaive,
  (.+),
  poly2vec,
  vec2poly,
  polyVecConstant,
  polyVecLinear,
  polyVecQuadratic,
  UnivariateRingPolynomial (..),
  UnivariateFieldPolynomial (..),
  UnivariateRingPolyVec (..),
  UnivariateFieldPolyVec (..),
) where

import Control.DeepSeq (NFData (..))
import Control.Monad (forM_)
import Data.Aeson (ToJSON)
import Data.Binary (Binary (..))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GHC.Generics (Generic)
import GHC.IsList (IsList (..))
import Test.QuickCheck (Arbitrary (..), chooseInt)
import Prelude hiding (Num (..), drop, length, product, replicate, sum, take, (/), (^))
import qualified Prelude as P

import ZkFold.Algebra.Class hiding (Euclidean (..))
import ZkFold.Algebra.DFT (genericDft)
import ZkFold.Algebra.Number
import ZkFold.Prelude (log2ceiling, replicate, zipVectorsWithDefault, zipWithDefault)

infixl 7 .*., ./.

infixl 6 .+, +.

-------------------------------- Arbitrary degree polynomials --------------------------------

class
  ( Ring c
  , AdditiveGroup poly
  ) =>
  UnivariateRingPolynomial c poly
    | poly -> c
  where
  constant :: c -> poly

  fromPoly :: poly -> V.Vector c

  toPoly :: V.Vector c -> poly

  -- | Leading coefficient
  lt :: poly -> c

  -- | Degree of zero polynomial is `-1`
  deg :: poly -> Integer

  -- | Return `c * x ^ n`
  monomial
    :: Natural
    -- ^ n
    -> c
    -- ^ c
    -> poly

  -- | Evaluate polynomial
  evalPoly :: poly -> c -> c

  -- | Return `c * p * (x ^ n)`
  scaleP
    :: c
    -- ^ c
    -> Natural
    -- ^ n
    -> poly
    -- ^ p
    -> poly

class
  ( Field c
  , Eq c
  , MultiplicativeMonoid poly
  , UnivariateRingPolynomial c poly
  ) =>
  UnivariateFieldPolynomial c poly
    | poly -> c
  where
  qr :: poly -> poly -> (poly, poly)

  -- | Extended Euclidean algorithm.
  eea :: poly -> poly -> (poly, poly)

instance
  ( Ring c
  , Eq c
  )
  => UnivariateRingPolynomial c (Poly c)
  where
  constant = P . V.singleton

  fromPoly (P cs) = cs

  lt (P cs) = V.last cs

  deg (P cs) = fromIntegral (V.length cs) - 1

  toPoly = P . removeZeros

  monomial d c = P $ V.fromList (replicate d zero P.<> [c])

  evalPoly (P cs) x = sum $ V.zipWith (*) cs $ fmap (x ^) (V.generate (V.length cs) (fromIntegral @_ @Natural))

  scaleP a n (P cs) = P $ V.replicate (fromIntegral n) zero V.++ fmap (a *) cs

instance
  ( Field c
  , Eq c
  )
  => UnivariateFieldPolynomial c (Poly c)
  where
  qr a b = go a b zero
   where
    go x y q = if deg x < deg y then (q, x) else go x' y q'
     where
      c = lt x // lt y
      n = fromIntegral (deg x - deg y)
      -- \^ if `deg x < deg y`, `n` is not evaluated, so this would not error out
      x' = x - scaleP c n y
      q' = q + scaleP c n one

  eea a b = go (a, one) (b, zero)
   where
    go (x, s) (y, t) = if deg y == -1 then (x, s) else go (y, t) (r, s - q * t)
     where
      (q, r) = qr x y

-- TODO (Issue #17): hide constructor
newtype Poly c = P (V.Vector c)
  deriving (Eq, Functor, Generic, NFData, Show)

instance {-# OVERLAPPING #-} FromConstant (Poly c) (Poly c)

instance FromConstant c c' => FromConstant c (Poly c') where
  fromConstant = P . V.singleton . fromConstant

instance (Ring c, Eq c) => AdditiveSemigroup (Poly c) where
  (P !l) + (P !r) = P $ removeZeros $ V.zipWith (+) lPadded rPadded
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
  -- \| If it is possible to calculate a primitive root of unity in the field, proceed with FFT multiplication.
  -- Otherwise default to Karatsuba multiplication for polynomials of degree higher than 64 or use naive multiplication otherwise.
  -- 64 is a threshold determined by benchmarking.
  (P !l) * (P !r) = P $ removeZeros $ mulAdaptive l r

padVector :: forall a. Ring a => V.Vector a -> Int -> V.Vector a
padVector v l
  | V.length v == l = v
  | otherwise = v V.++ V.replicate (l P.- V.length v) zero

mulAdaptive :: forall c. (Field c, Eq c) => V.Vector c -> V.Vector c -> V.Vector c
mulAdaptive !l !r
  | V.null l = V.empty
  | V.null r = V.empty
  | Just (m, cm, c0) <- isShiftedMono r =
      V.generate (V.length l P.+ V.length r) $ mulShiftedMonoIx l (fromIntegral m) cm c0
  | Just (m, cm, c0) <- isShiftedMono l =
      V.generate (V.length l P.+ V.length r) $ mulShiftedMonoIx r (fromIntegral m) cm c0
  | otherwise =
      case (maybeW2n, resultLen <= 64) of
        (_, True) -> mulVector l r
        (Just w2n, _) -> mulDft dftP w2n lDft rDft
        (Nothing, False) -> mulKaratsuba lKaratsuba rKaratsuba
 where
  mulShiftedMonoIx :: V.Vector c -> Int -> c -> c -> Int -> c
  mulShiftedMonoIx ref m cm c0 ix =
    let scaled = if ix < V.length ref then c0 * (ref V.! ix) else zero
        shifted = if ix >= m && ix P.- m < V.length ref then cm * (ref V.! (ix P.- m)) else zero
     in scaled + shifted

  -------------------------------------------------------------------------------------------------
  -- DFT
  -------------------------------------------------------------------------------------------------

  resultLen :: Int
  resultLen = V.length l P.+ V.length r P.- 1

  dftP :: Integer
  dftP = ceiling @Double $ logBase 2 (fromIntegral resultLen)

  padDft :: Int
  padDft = 2 P.^ dftP

  lDft, rDft :: V.Vector c
  lDft = padVector l padDft
  rDft = padVector r padDft

  maybeW2n :: Maybe c
  maybeW2n = rootOfUnity $ fromIntegral dftP

  -------------------------------------------------------------------------------------------------
  -- Karatsuba
  -------------------------------------------------------------------------------------------------

  maxLen :: Int
  maxLen = max (V.length l) (V.length r)

  karatsubaP :: Integer
  karatsubaP = ceiling @Double $ logBase 2 (fromIntegral maxLen)

  padKaratsuba :: Int
  padKaratsuba = 2 P.^ karatsubaP

  lKaratsuba, rKaratsuba :: V.Vector c
  lKaratsuba = padVector l padKaratsuba
  rKaratsuba = padVector r padKaratsuba

mulDft :: forall c. Field c => Integer -> c -> V.Vector c -> V.Vector c -> V.Vector c
mulDft !p !w2n !lPadded !rPadded = c
 where
  pad :: Int
  !pad = 2 P.^ p

  w2nInv :: c
  !w2nInv = one // w2n

  nInv :: c
  !nInv = one // fromConstant (fromIntegral @_ @Natural pad)

  v1Image, v2Image :: V.Vector c
  !v1Image = genericDft p w2n lPadded
  !v2Image = genericDft p w2n rPadded

  cImage :: V.Vector c
  !cImage = V.zipWith (*) v1Image v2Image

  c :: V.Vector c
  !c = (* nInv) <$> genericDft p w2nInv cImage

mulKaratsuba :: forall a. (Eq a, Field a) => V.Vector a -> V.Vector a -> V.Vector a
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
  ix2v' (-1) _ accum = accum
  ix2v' _ ((== len2) -> True) accum = accum
  ix2v' i j accum = ix2v' (i P.- 1) (j P.+ 1) (accum + v1 `V.unsafeIndex` i * v2 `V.unsafeIndex` j)

instance (Field c, Eq c) => Exponent (Poly c) Natural where
  (^) = natPow

instance (Field c, Eq c) => MultiplicativeMonoid (Poly c) where
  one = P $ V.singleton one

instance (Field c, Eq c) => Semiring (Poly c)

instance (Field c, Eq c) => Ring (Poly c)

instance (Ring c, Arbitrary c, Eq c) => Arbitrary (Poly c) where
  arbitrary = fmap toPoly $ chooseInt (0, 128) >>= \n -> V.replicateM n arbitrary

---------------------------------- Fixed degree polynomials ----------------------------------

newtype PolyVec c (size :: Natural) = PV (V.Vector c)
  deriving (Eq, Generic, NFData, Show, ToJSON)

instance Binary c => Binary (PolyVec c size) where
  put (PV v) = put $ V.toList v
  get = PV . V.fromList <$> get

class
  ( Ring c
  , forall size. KnownNat size => AdditiveGroup (pv size)
  , forall size. KnownNat size => Scale c (pv size)
  ) =>
  UnivariateRingPolyVec c pv
    | pv -> c
  where
  -- | Multiply the corresponding coefficients of two polynomials.
  (.*.) :: KnownNat size => pv size -> pv size -> pv size

  -- | Add a constant to every coefficient of the polynomial.
  (+.) :: KnownNat size => c -> pv size -> pv size

  -- | (toPolyVec [a0, ..., an])(x) == a0 + ... + an x^n
  toPolyVec :: KnownNat size => V.Vector c -> pv size

  -- | fromPolyVec (\x -> a0 + ... + an x^n) == [a0, ..., an].
  --
  --   NOTE: size of the vector is implementation-defined.
  fromPolyVec :: KnownNat size => pv size -> V.Vector c

  -- | evalPolyVec p x = p(x)
  evalPolyVec :: KnownNat size => pv size -> c -> c

-- | Flipped version of a '+.'.
--
-- NOTE: we do not distinguish between left and right additive actions
-- since all our (additive) groups are abelian.
(.+) :: (UnivariateRingPolyVec c pv, KnownNat size) => pv size -> c -> pv size
(.+) = flip (+.)

poly2vec
  :: (UnivariateRingPolynomial c poly, UnivariateRingPolyVec c pv)
  => KnownNat size
  => poly -> pv size
poly2vec = toPolyVec . fromPoly

vec2poly
  :: (UnivariateRingPolyVec c pv, UnivariateRingPolynomial c poly)
  => KnownNat size
  => pv size -> poly
vec2poly = toPoly . fromPolyVec

-- | (polyVecConstant a0)(x) = a0
polyVecConstant :: (UnivariateRingPolyVec c pv, KnownNat size) => c -> pv size
polyVecConstant a0 = toPolyVec (V.singleton a0)

-- | (polyVecLinear a1 a0)(x) = a1 * x + a0
polyVecLinear
  :: (UnivariateRingPolyVec c pv, KnownNat size) => c -> c -> pv size
polyVecLinear a1 a0 = toPolyVec (V.fromList [a0, a1])

-- | (polyVecQuadratic a2 a1 a0)(x) = a2 * x^2 + a1 * x + a0
polyVecQuadratic
  :: (UnivariateRingPolyVec c pv, KnownNat size) => c -> c -> c -> pv size
polyVecQuadratic a2 a1 a0 = toPolyVec (V.fromList [a0, a1, a2])

class
  ( Field c
  , Eq c
  , forall size. KnownNat size => Ring (pv size)
  , UnivariateRingPolyVec c pv
  ) =>
  UnivariateFieldPolyVec c pv
    | pv -> c
  where
  -- | Divide the corresponding coefficients of two polynomials.
  (./.) :: forall size. KnownNat size => pv size -> pv size -> pv size

  -- | p(x) = x^n - 1
  polyVecZero :: forall size. KnownNat size => Natural -> pv size

  -- | L_i(x) : p(omega^i) = 1, p(omega^j) = 0, j /= i, 1 <= i <= n, 1 <= j <= n
  polyVecLagrange :: forall size. KnownNat size => Natural -> Natural -> c -> pv size

  -- | p(x) = c_1 * L_1(x) + c_2 * L_2(x) + ... + c_n * L_n(x)
  polyVecInLagrangeBasis :: forall n size. (KnownNat size, KnownNat n) => c -> pv n -> pv size

  polyVecGrandProduct :: forall size. KnownNat size => pv size -> pv size -> pv size -> c -> c -> pv size

  polyVecDiv :: forall size. KnownNat size => pv size -> pv size -> pv size

  castPolyVec :: forall size size'. (KnownNat size, KnownNat size') => pv size -> pv size'

instance
  ( Ring c
  , Eq c
  )
  => UnivariateRingPolyVec c (PolyVec c)
  where
  (PV l) .*. (PV r) = toPolyVec $ V.zipWith (*) l r

  (+.) :: forall size. KnownNat size => c -> PolyVec c size -> PolyVec c size
  a +. (PV cs) = PV $ fmap (+ a) (addZeros @c @size cs)

  toPolyVec :: forall size. KnownNat size => V.Vector c -> PolyVec c size
  toPolyVec = PV . V.take (fromIntegral $ value @size) . removeZeros

  fromPolyVec :: forall size. KnownNat size => PolyVec c size -> V.Vector c
  fromPolyVec (PV cs) = addZeros @c @size cs

  evalPolyVec :: forall size. PolyVec c size -> c -> c
  evalPolyVec (PV cs) x = sum $ V.zipWith (*) cs $ fmap (x ^) (V.generate (V.length cs) (fromIntegral @_ @Natural))

instance
  ( Field c
  , Eq c
  , UnivariateRingPolyVec c (PolyVec c)
  )
  => UnivariateFieldPolyVec c (PolyVec c)
  where
  (PV l) ./. (PV r) = PV $ V.zipWith (//) l r

  polyVecZero :: forall size. KnownNat size => Natural -> PolyVec c size
  polyVecZero n = poly2vec $ scaleP one n (one @(Poly c)) - one @(Poly c)

  polyVecLagrange :: forall size. KnownNat size => Natural -> Natural -> c -> PolyVec c size
  polyVecLagrange n i omega = toPolyVec $ V.unfoldrExactN vecLen coefficients (norm * wi ^ (n -! 1), n)
   where
    wi = omega ^ i

    wInv = one // wi

    norm = wi // fromConstant n

    vecLen = fromIntegral n

    coefficients (_, 0) = (zero, (zero, 0))
    coefficients (w, ix) = (w, (w * wInv, ix -! 1))

  polyVecInLagrangeBasis :: forall n size. (KnownNat n, KnownNat size) => c -> PolyVec c n -> PolyVec c size
  polyVecInLagrangeBasis omega (PV cs') = toPolyVec $ V.reverse dft
   where
    cs = addZeros @c @n cs'

    nInt :: P.Int
    nInt = fromIntegral $ value @n

    norms = V.generate (V.length cs) $ \ix -> omega ^ (fromIntegral ix :: Natural) // fromConstant (value @n)

    cyc = V.backpermute cs $ V.generate (V.length cs) (\ix -> pred ix `P.mod` nInt)
    dft = genericDft (log2ceiling $ value @n) omega $ V.zipWith (*) norms cyc

  polyVecGrandProduct
    :: forall size. KnownNat size => PolyVec c size -> PolyVec c size -> PolyVec c size -> c -> c -> PolyVec c size
  polyVecGrandProduct as bs sigmas beta gamma =
    let ps = gamma +. (as + beta *. bs)
        qs = gamma +. (as + beta *. sigmas)
        zs = fmap (product . flip V.take (fromPolyVec $ ps ./. qs)) (V.generate (fromIntegral (value @size)) id)
     in PV zs

  -- Special case: @r@ == ax^m + b, m > 0 (or 'shifted monomial')
  -- Then, division can be performed in O(size)
  --
  polyVecDiv :: forall size. KnownNat size => PolyVec c size -> PolyVec c size -> PolyVec c size
  polyVecDiv l r@(PV rcs)
    | Just (m, cm, c0) <- isShiftedMono rcs = divShiftedMono (l .* finv cm) m c0
    | otherwise = poly2vec $ fst $ qr @c @(Poly c) (vec2poly l) (vec2poly r)

  castPolyVec :: forall size size'. (KnownNat size, KnownNat size') => PolyVec c size -> PolyVec c size'
  castPolyVec (PV cs)
    | value @size <= value @size' = toPolyVec cs
    | all (== zero) (V.drop (fromIntegral (value @size')) cs) = toPolyVec cs
    | otherwise = error "castPolyVec: Cannot cast polynomial vector to smaller size!"

-- | Determines whether a polynomial is of the form 'ax^m + b' (m > 0) and returns @Just (m, a, b)@ if so.
-- Multiplication and division by polynomials of such form can be performed much faster than with general algorithms.
isShiftedMono :: forall c. (Field c, Eq c) => V.Vector c -> Maybe (Natural, c, c)
isShiftedMono cs
  | V.length filtered /= 2 = Nothing
  | otherwise = case V.toList filtered of
      [(c0, 0), (cm, m)] -> pure (m, cm, c0)
      _ -> Nothing
 where
  ixed :: V.Vector (c, Natural)
  ixed = V.zip cs $ V.iterateN (V.length cs) succ 0

  filtered :: V.Vector (c, Natural)
  filtered = V.filter ((/= zero) . fst) ixed

-- | Efficiently divide a polynomial by a monic 'shifted monomial' of the form x^m + b, m > 0
-- The remainder is discarded.
-- The algorithm requires (size - m) field multiplications.
--
-- Division is performed from higher degrees downwards.
--
-- i-th step of the algorithm:
--
--  ci * x^i =
--  ci * x^i + ci * x^(i-m) * b - ci * x^(i-m) * b =
--  ci * x^(i-m) * (x*m + b) - ci * x^(i-m) * b
--
--  > set the (i-m)-th coefficient of the result to be @ci@
--  > Subtract @ci * b@ from the (i-m)-th coefficient of the numerator
--  > Proceed to degree @i-1@
divShiftedMono :: forall c size. Field c => PolyVec c size -> Natural -> c -> PolyVec c size
divShiftedMono (PV cs) m b = PV $ V.create $ do
  let intLen = V.length cs
      intM = fromIntegral m
  c <- V.thaw cs
  res <- VM.replicate intLen zero
  forM_ [intLen P.- 1, intLen P.- 2 .. intM] $ \ix -> do
    ci <- VM.read c ix
    VM.write res (ix P.- intM) ci
    VM.modify c (\x -> x - ci * b) (ix P.- intM)
  pure res

instance
  ( Ring c
  , KnownNat size
  )
  => IsList (PolyVec c size)
  where
  type Item (PolyVec c size) = c
  fromList = PV . V.fromList
  toList (PV cs) = V.toList (addZeros @c @size cs)

instance Scale c' c => Scale c' (PolyVec c size) where
  scale c (PV p) = PV (scale c <$> p)

instance FromConstant Natural c => FromConstant Natural (PolyVec c size) where
  fromConstant n = PV $ V.singleton (fromConstant n)

instance FromConstant Integer c => FromConstant Integer (PolyVec c size) where
  fromConstant n = PV $ V.singleton (fromConstant n)

instance (Ring c, Eq c, KnownNat size) => AdditiveSemigroup (PolyVec c size) where
  PV l + PV r = toPolyVec $ zipVectorsWithDefault zero (+) l r

instance (Ring c, Eq c, KnownNat size) => AdditiveMonoid (PolyVec c size) where
  zero = PV V.empty

instance (Ring c, Eq c, KnownNat size) => AdditiveGroup (PolyVec c size) where
  negate (PV cs) = PV $ fmap negate cs

instance (Field c, Eq c, KnownNat size) => Exponent (PolyVec c size) Natural where
  (^) = natPow

instance {-# OVERLAPPING #-} (Field c, KnownNat size, Eq c) => Scale (PolyVec c size) (PolyVec c size)

-- TODO (Issue #18): check for overflow
instance (Field c, Eq c, KnownNat size) => MultiplicativeSemigroup (PolyVec c size) where
  (PV l) * (PV r) = toPolyVec $ mulAdaptive (removeZeros l) (removeZeros r)

instance (Field c, Eq c, KnownNat size) => MultiplicativeMonoid (PolyVec c size) where
  one = PV $ V.singleton one

instance (Field c, KnownNat size, Eq c) => Semiring (PolyVec c size)

instance (Field c, KnownNat size, Eq c) => Ring (PolyVec c size)

instance (Ring c, Arbitrary c, Eq c, KnownNat size) => Arbitrary (PolyVec c size) where
  arbitrary = toPolyVec @_ @(PolyVec c) <$> V.replicateM (fromIntegral $ value @size) (arbitrary @c)

-------------------------------- Helper functions --------------------------------

rewrapPolyVec :: (V.Vector c -> V.Vector c) -> PolyVec c size -> PolyVec c size
rewrapPolyVec f (PV x) = PV (f x)

removeZeros :: (Ring c, Eq c) => V.Vector c -> V.Vector c
removeZeros !cs
  | V.null cs = cs
  | otherwise = V.take (1 P.+ traverseZeros startIx) cs
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

addZeros :: forall c size. (Ring c, KnownNat size) => V.Vector c -> V.Vector c
addZeros cs = cs V.++ V.replicate (fromIntegral (value @size) P.- V.length cs) zero

-- ** THE CODE BELOW IS ONLY USED FOR BENCHMARKING MULTIPLICATION **

-- | Naive vector multiplication, O(n^2)
mulPoly :: forall a. Field a => Poly a -> Poly a -> Poly a
mulPoly (P v1) (P v2) = P $ mulVector v1 v2

-- | Adaptation of Karatsuba's algorithm. O(n^log_2(3))
mulPolyKaratsuba :: (Eq a, Field a) => Poly a -> Poly a -> Poly a
mulPolyKaratsuba (P v1) (P v2) = P $ removeZeros result
 where
  l = max (V.length v1) (V.length v2)
  p = ceiling @Double @Integer $ logBase 2 (fromIntegral l)

  pad = 2 P.^ p

  result =
    mulKaratsuba
      (v1 V.++ V.replicate (pad P.- V.length v1) zero)
      (v2 V.++ V.replicate (pad P.- V.length v2) zero)

-- DFT multiplication of vectors. O(nlogn)
--
mulPolyDft :: forall a. (Eq a, Field a) => Poly a -> Poly a -> Poly a
mulPolyDft (P v1) (P v2) = P $ removeZeros result
 where
  l = max (V.length v1) (V.length v2)
  p = (ceiling @Double $ logBase 2 (fromIntegral l)) P.+ 1

  w2n :: a
  w2n = case rootOfUnity $ fromIntegral p of
    Just a -> a
    _ -> undefined

  pad = 2 P.^ p

  result =
    mulDft @a
      p
      w2n
      (v1 V.++ V.replicate (pad P.- V.length v1) zero)
      (v2 V.++ V.replicate (pad P.- V.length v2) zero)

mulPolyNaive :: Field a => Poly a -> Poly a -> Poly a
mulPolyNaive (P v1) (P v2) = P $ V.fromList $ go (V.toList v1) (V.toList v2)
 where
  go [] _ = []
  go (x : xs) ys = zipWithDefault (+) zero zero (map (x *) ys) (zero : go xs ys)
