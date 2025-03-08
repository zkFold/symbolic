{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Algebra.Basic.Field (
    IrreduciblePoly(..),
    Zp,
    toZp,
    fromZp,
    inv,
    Ext2(..),
    Ext3(..)
    ) where

import           Control.Applicative                        ((<|>))
import           Control.DeepSeq                            (NFData (..))
import           Data.Aeson                                 (FromJSON (..), ToJSON (..))
import           Data.Bifunctor                             (first)
import           Data.Bool                                  (bool)
import qualified Data.Vector                                as V
import           GHC.Generics                               (Generic)
import           GHC.Real                                   ((%))
import           GHC.TypeLits                               (Symbol)
import           Prelude                                    hiding (Fractional (..), Num (..), div, length, (^))
import qualified Prelude                                    as Haskell
import           System.Random                              (Random (..), RandomGen, mkStdGen, uniformR)
import           Test.QuickCheck                            hiding (scale)

import           ZkFold.Base.Algebra.Basic.Class            hiding (Euclidean (..))
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.Polynomials.Univariate
import           ZkFold.Base.Data.ByteString
import           ZkFold.Prelude                             (log2ceiling)

------------------------------ Prime Fields -----------------------------------

newtype Zp (p :: Natural) = Zp Integer
    deriving (Generic, NFData)

{-# INLINE fromZp #-}
fromZp :: Zp p -> Natural
fromZp (Zp a) = fromIntegral a

{-# INLINE residue #-}
residue :: forall p . KnownNat p => Integer -> Integer
residue = (`Haskell.mod` fromIntegral (value @p))

{-# INLINE toZp #-}
toZp :: forall p . KnownNat p => Integer -> Zp p
toZp = Zp . residue @p

instance ToConstant (Zp p) where
    type Const (Zp p) = Natural
    toConstant = fromZp

instance (KnownNat p, KnownNat (NumberOfBits (Zp p))) => Finite (Zp p) where
    type Order (Zp p) = p

instance KnownNat p => Eq (Zp p) where
    Zp a == Zp b = residue @p (a - b) == 0

instance KnownNat p => Ord (Zp p) where
    Zp a <= Zp b = residue @p a <= residue @p b

instance KnownNat p => AdditiveSemigroup (Zp p) where
    Zp a + Zp b = toZp (a + b)

instance KnownNat p => Scale Natural (Zp p) where
    scale c (Zp a) = toZp (scale c a)

instance KnownNat p => AdditiveMonoid (Zp p) where
    zero = Zp 0

instance KnownNat p => Scale Integer (Zp p) where
    scale c (Zp a) = toZp (scale c a)

instance KnownNat p => AdditiveGroup (Zp p) where
    negate (Zp a) = toZp (negate a)
    Zp a - Zp b   = toZp (a - b)

instance KnownNat p => MultiplicativeSemigroup (Zp p) where
    Zp a * Zp b = toZp (a * b)

instance KnownNat p => Exponent (Zp p) Natural where
    (^) = natPow

instance KnownNat p => MultiplicativeMonoid (Zp p) where
    one = Zp 1

instance KnownNat p => FromConstant Natural (Zp p) where
    fromConstant = toZp . fromConstant

instance KnownNat p => Semiring (Zp p)

instance KnownNat p => SemiEuclidean (Zp p) where
    divMod a b = let (q, r) = Haskell.divMod (fromZp a) (fromZp b)
                  in (toZp . fromIntegral $ q, toZp . fromIntegral $ r)

instance KnownNat p => FromConstant Integer (Zp p) where
    fromConstant = toZp

instance KnownNat p => Ring (Zp p)

instance Prime p => Exponent (Zp p) Integer where
    -- | By Fermat's little theorem
    a ^ n = intPowF a (n `Haskell.mod` (fromConstant (value @p) - 1))

instance Prime p => Field (Zp p) where
    finv (Zp a) = fromConstant $ inv a (value @p)

    rootOfUnity l
      | l == 0                       = Nothing
      | (value @p -! 1) `Haskell.mod` n /= 0 = Nothing
      | otherwise = Just $ rootOfUnity' (mkStdGen 0)
        where
          n = 2 ^ l
          rootOfUnity' :: RandomGen g => g -> Zp p
          rootOfUnity' g =
              let (x, g') = first fromConstant $ uniformR (1, value @p -! 1) g
                  x' = x ^ ((value @p -! 1) `Haskell.div` n)
              in bool (rootOfUnity' g') x' (x' ^ (n `Haskell.div` 2) /= one)

inv :: Integer -> Natural -> Natural
inv a p = fromIntegral $ snd (egcd (a, 1) (fromConstant p, 0)) `Haskell.mod` fromConstant p
  where
    egcd (x, y) (0, _) = (x, y)
    egcd (x, y) (x', y') = egcd (x', y') (x - q * x', y - q * y')
      where q = x `div` x'

instance Prime p => BinaryExpansion (Zp p) where
    type Bits (Zp p) = [Zp p]
    binaryExpansion = map (Zp . fromConstant) . binaryExpansion . fromZp

instance Prime p => DiscreteField' (Zp p)

instance Prime p => TrichotomyField (Zp p)

instance KnownNat p => Haskell.Num (Zp p) where
    fromInteger = toZp
    (+)         = (+)
    (-)         = (-)
    (*)         = (*)
    negate      = negate
    abs         = id
    signum      = const 1

instance Prime p => Haskell.Fractional (Zp p) where
    fromRational = error "`fromRational` is not implemented for `Zp p`"
    recip        = finv
    (/)          = (//)

instance Show (Zp p) where
    show (Zp a) = show a

instance ToJSON (Zp p) where
    toJSON (Zp a) = toJSON a

instance FromJSON (Zp p) where
    parseJSON = fmap Zp . parseJSON

instance KnownNat p => Binary (Zp p) where
    put (Zp x) = go x (wordCount @p)
      where
        go _ 0      = pure ()
        go n count =
            let (n', r) = n `Haskell.divMod` 256
            in putWord8 (fromIntegral r) <> go n' (count -! 1)
    get = toZp <$> go (wordCount @p)
      where
        go 0 = pure zero
        go n = liftA2 combine getWord8 (go $ n -! 1) <|> pure zero
        combine r d = fromIntegral r + 256 * d

wordCount :: forall p. KnownNat p => Natural
wordCount = ceiling $ log2ceiling (value @p) % (8 :: Natural)

instance KnownNat p => Arbitrary (Zp p) where
    arbitrary = toZp <$> chooseInteger (0, fromIntegral (value @p) - 1)

instance KnownNat p => Random (Zp p) where
    randomR (Zp a, Zp b) g = (Zp r, g')
      where
        (r, g') = randomR (a, b) g

    random g = (Zp r, g')
      where
        (r, g') = randomR (0, fromIntegral (value @p) - 1) g

-- | Exponentiation by an element of a finite field is well-defined (and lawful)
-- if and only if the base is a finite multiplicative group of a matching order.
--
-- Note that left distributivity is satisfied, meaning
-- @a ^ (m + n) = (a ^ m) * (a ^ n)@.
instance (MultiplicativeGroup a, Order a ~ p) => Exponent a (Zp p) where
    a ^ n = a ^ fromZp n

----------------------------- Field Extensions --------------------------------

class IrreduciblePoly f (e :: Symbol) | e -> f where
    irreduciblePoly :: Poly f

data Ext2 f (e :: Symbol) = Ext2 f f
    deriving (Eq, Show, Generic)

instance Ord f => Ord (Ext2 f e) where
    Ext2 a b <= Ext2 c d = [b, a] <= ([d, c] :: [f])

instance (KnownNat (Order (Ext2 f e)), KnownNat (NumberOfBits (Ext2 f e))) => Finite (Ext2 f e) where
    type Order (Ext2 f e) = Order f ^ 2

instance {-# OVERLAPPING #-} FromConstant (Ext2 f e) (Ext2 f e)

instance Field f => AdditiveSemigroup (Ext2 f e) where
    Ext2 a b + Ext2 c d = Ext2 (a + c) (b + d)

instance Scale c f => Scale c (Ext2 f e) where
    scale c (Ext2 a b) = Ext2 (scale c a) (scale c b)

instance Field f => AdditiveMonoid (Ext2 f e) where
    zero = Ext2 zero zero

instance Field f => AdditiveGroup (Ext2 f e) where
    negate (Ext2 a b) = Ext2 (negate a) (negate b)
    Ext2 a b - Ext2 c d = Ext2 (a - c) (b - d)

instance {-# OVERLAPPING #-} (Field f, Eq f, IrreduciblePoly f e) => Scale (Ext2 f e) (Ext2 f e)

instance (Field f, Eq f, IrreduciblePoly f e) => MultiplicativeSemigroup (Ext2 f e) where
    Ext2 a b * Ext2 c d = fromConstant (toPoly [a, b] * toPoly [c, d])

instance MultiplicativeMonoid (Ext2 f e) => Exponent (Ext2 f e) Natural where
    (^) = natPow

instance (Field f, Eq f, IrreduciblePoly f e) => MultiplicativeMonoid (Ext2 f e) where
    one = Ext2 one zero

instance Field (Ext2 f e) => Exponent (Ext2 f e) Integer where
    (^) = intPowF

instance (Field f, Eq f, IrreduciblePoly f e) => Field (Ext2 f e) where
    finv (Ext2 a b) =
        let (g, s) = eea (toPoly [a, b]) (irreduciblePoly @f @e)
        in case fromPoly $ scaleP (one // lt g) 0 s of
            []  -> Ext2 zero zero
            [x] -> Ext2 x zero
            v   -> Ext2 (v V.! 0) (v V.! 1)

    rootOfUnity n = (\r -> Ext2 r zero) <$> rootOfUnity n

instance (FromConstant f f', Field f') => FromConstant f (Ext2 f' e) where
    fromConstant e = Ext2 (fromConstant e) zero

instance {-# OVERLAPPING #-} (Field f, Eq f, IrreduciblePoly f e) => FromConstant (Poly f) (Ext2 f e) where
    fromConstant p = case fromPoly . snd $ qr p (irreduciblePoly @f @e) of
      []  -> zero
      [x] -> fromConstant x
      v   -> Ext2 (v V.! 0) (v V.! 1)

instance (Field f, Eq f, IrreduciblePoly f e) => Semiring (Ext2 f e)

instance (Field f, Eq f, IrreduciblePoly f e) => Ring (Ext2 f e)

instance Binary f => Binary (Ext2 f e) where
    put (Ext2 a b) = put a <> put b
    get = Ext2 <$> get <*> get

instance (Field f, Eq f, IrreduciblePoly f e, Arbitrary f) => Arbitrary (Ext2 f e) where
    arbitrary = Ext2 <$> arbitrary <*> arbitrary

data Ext3 f (e :: Symbol) = Ext3 f f f
    deriving (Eq, Show, Generic)

instance Ord f => Ord (Ext3 f e) where
    Ext3 a b c <= Ext3 d e f = [c, b, a] <= ([f, e, d] :: [f])

instance (KnownNat (Order (Ext3 f e)), KnownNat (NumberOfBits (Ext3 f e))) => Finite (Ext3 f e) where
    type Order (Ext3 f e) = Order f ^ 3

instance {-# OVERLAPPING #-} FromConstant (Ext3 f e) (Ext3 f e)

instance Field f => AdditiveSemigroup (Ext3 f e) where
    Ext3 a b c + Ext3 d e f = Ext3 (a + d) (b + e) (c + f)

instance Scale c f => Scale c (Ext3 f e) where
    scale c (Ext3 d e f) = Ext3 (scale c d) (scale c e) (scale c f)

instance Field f => AdditiveMonoid (Ext3 f e) where
    zero = Ext3 zero zero zero

instance Field f => AdditiveGroup (Ext3 f e) where
    negate (Ext3 a b c) = Ext3 (negate a) (negate b) (negate c)
    Ext3 a b c - Ext3 d e f = Ext3 (a - d) (b - e) (c - f)

instance {-# OVERLAPPING #-} (Field f, Eq f, IrreduciblePoly f e) => Scale (Ext3 f e) (Ext3 f e)

instance (Field f, Eq f, IrreduciblePoly f e) => MultiplicativeSemigroup (Ext3 f e) where
    Ext3 a b c * Ext3 d e f = fromConstant (toPoly [a, b, c] * toPoly [d, e, f])

instance MultiplicativeMonoid (Ext3 f e) => Exponent (Ext3 f e) Natural where
    (^) = natPow

instance (Field f, Eq f, IrreduciblePoly f e) => MultiplicativeMonoid (Ext3 f e) where
    one = Ext3 one zero zero

instance Field (Ext3 f e) => Exponent (Ext3 f e) Integer where
    (^) = intPowF

instance (Field f, Eq f, IrreduciblePoly f e) => Field (Ext3 f e) where
    finv (Ext3 a b c) =
        let (g, s) = eea (toPoly [a, b, c]) (irreduciblePoly @f @e)
        in case fromPoly $ scaleP (one // lt g) 0 s of
            []     -> Ext3 zero zero zero
            [x]    -> Ext3 x zero zero
            [x, y] -> Ext3 x y zero
            v      -> Ext3 (v V.! 0) (v V.! 1) (v V.! 2)

    rootOfUnity n = (\r -> Ext3 r zero zero) <$> rootOfUnity n

instance (FromConstant f f', Field f') => FromConstant f (Ext3 f' ip) where
    fromConstant e = Ext3 (fromConstant e) zero zero

instance {-# OVERLAPPING #-} (Field f, Eq f, IrreduciblePoly f e) => FromConstant (Poly f) (Ext3 f e) where
    fromConstant p = case fromPoly . snd $ qr p (irreduciblePoly @f @e) of
      []     -> zero
      [x]    -> fromConstant x
      [x, y] -> Ext3 x y zero
      v      -> Ext3 (v V.! 0) (v V.! 1) (v V.! 2)

instance (Field f, Eq f, IrreduciblePoly f e) => Semiring (Ext3 f e)

instance (Field f, Eq f, IrreduciblePoly f e) => Ring (Ext3 f e)

instance Binary f => Binary (Ext3 f e) where
    put (Ext3 a b c) = put a <> put b <> put c
    get = Ext3 <$> get <*> get <*> get

instance (Field f, Eq f, IrreduciblePoly f e, Arbitrary f) => Arbitrary (Ext3 f e) where
    arbitrary = Ext3 <$> arbitrary <*> arbitrary <*> arbitrary
