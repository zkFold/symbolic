{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Algebra.Field (
  IrreduciblePoly (..),
  Zp,
  toZp,
  fromZp,
  inv,
  Ext2 (..),
  Ext3 (..),
) where

import Control.Applicative (liftA2, pure, (<*>), (<|>))
import Control.DeepSeq (NFData (..))
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import Data.Bool (Bool)
import Data.Function (const, id, ($), (.))
import Data.Functor (fmap, (<$>))
import Data.List ((++))
import Data.Maybe (Maybe (..))
import Data.OpenApi (ToSchema)
import Data.Semigroup ((<>))
import Data.Tuple (snd)
import Data.Type.Equality (type (~))
import qualified Data.Vector as V
import GHC.Generics (Generic)
import GHC.Natural (powModNatural)
import GHC.Real ((%))
import GHC.TypeLits (Symbol)
import System.Random (Random (..))
import System.Random.Stateful (Uniform (..), UniformRange (..))
import Test.QuickCheck hiding (scale)
import Prelude (Integer)
import qualified Prelude as Haskell

import ZkFold.Algebra.Class hiding (Euclidean (..))
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Univariate
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Binary
import ZkFold.Data.Eq
import ZkFold.Data.Ord (Ord)
import ZkFold.Prelude (iterate', log2ceiling)

------------------------------ Prime Fields -----------------------------------

newtype Zp (p :: Natural) = Zp Integer
  deriving Generic
  deriving newtype (FromJSONKey, NFData, ToJSONKey, ToSchema)

{-# INLINE fromZp #-}
fromZp :: Zp p -> Natural
fromZp (Zp a) = Haskell.fromIntegral a

{-# INLINE residue #-}
residue :: forall p. KnownNat p => Integer -> Integer
residue = (`Haskell.mod` Haskell.fromIntegral (value @p))

{-# INLINE toZp #-}
toZp :: forall p. KnownNat p => Integer -> Zp p
toZp = Zp . residue @p

instance ToConstant (Zp p) where
  type Const (Zp p) = Natural
  toConstant = fromZp

instance (KnownNat p, KnownNat (NumberOfBits (Zp p))) => Finite (Zp p) where
  type Order (Zp p) = p

instance KnownNat p => Haskell.Eq (Zp p) where
  Zp a == Zp b = residue @p (a - b) == 0

instance KnownNat p => Haskell.Ord (Zp p) where
  Zp a <= Zp b = residue @p a Haskell.<= residue @p b

deriving via (HaskellEqOrd (Zp n)) instance KnownNat n => Ord (Zp n)

instance KnownNat p => Haskell.Enum (Zp p) where
  succ = (+ one)
  pred x = x - one
  toEnum = Zp . Haskell.toEnum
  fromEnum (Zp x) = Haskell.fromEnum x
  enumFrom = Haskell.enumFromThen <*> Haskell.succ
  enumFromThen x x' = let !d = x' - x in iterate' (+ d) x
  enumFromTo = Haskell.enumFromThenTo <*> Haskell.succ
  enumFromThenTo x x' y =
    Haskell.takeWhile (/= y) (Haskell.enumFromThen x x') ++ [y]

instance KnownNat p => AdditiveSemigroup (Zp p) where
  Zp a + Zp b = toZp (a + b)

instance KnownNat p => Scale Natural (Zp p) where
  scale c (Zp a) = toZp (scale c a)

instance Zero (Zp p) where
  zero = Zp 0

instance KnownNat p => AdditiveMonoid (Zp p)

instance KnownNat p => Scale Integer (Zp p) where
  scale c (Zp a) = toZp (scale c a)

instance KnownNat p => AdditiveGroup (Zp p) where
  negate (Zp a) = toZp (negate a)
  Zp a - Zp b = toZp (a - b)

instance KnownNat p => MultiplicativeSemigroup (Zp p) where
  Zp a * Zp b = toZp (a * b)
  square (Zp a) = Zp $ Haskell.fromIntegral $ powModNatural (Haskell.fromIntegral a) 2 (value @p)

instance KnownNat p => Exponent (Zp p) Natural where
  (Zp z) ^ n = Zp $ Haskell.fromIntegral $ powModNatural (Haskell.fromIntegral z) n (value @p)

instance KnownNat p => MultiplicativeMonoid (Zp p) where
  one = Zp 1

instance KnownNat p => FromConstant Natural (Zp p) where
  fromConstant = toZp . fromConstant

instance KnownNat p => Semiring (Zp p)

instance KnownNat p => SemiEuclidean (Zp p) where
  divMod a b =
    let (q, r) = Haskell.divMod (fromZp a) (fromZp b)
     in ( toZp (Haskell.fromIntegral q)
        , toZp (Haskell.fromIntegral r)
        )

instance KnownNat p => FromConstant Integer (Zp p) where
  fromConstant = toZp

instance KnownNat p => Ring (Zp p)

instance Prime p => Exponent (Zp p) Integer where
  -- \| By Fermat's little theorem
  a ^ n = a ^ (Haskell.fromIntegral $ n `Haskell.mod` (fromConstant (value @p) - 1) :: Natural)

instance KnownNat n => Eq (Zp n) where
  type BooleanOf (Zp n) = Bool
  (==) = (Haskell.==)
  (/=) = (Haskell./=)

instance Prime p => Field (Zp p) where
  --    finv (Zp a) = fromConstant $ inv a (value @p)
  finv zp = zp ^ (value @p -! 2)

  rootOfUnity l
    | l == 0 = Nothing
    | orderNotDivisible = Nothing
    | Haskell.otherwise = Just $ rootOfUnity' 2
   where
    orderNotDivisible = (value @p -! 1) `Haskell.mod` n /= 0
    n = 2 ^ l
    rootOfUnity' :: Natural -> Zp p
    rootOfUnity' g =
      let x = fromConstant g
          x' = x ^ ((value @p -! 1) `Haskell.div` n)
       in bool (rootOfUnity' (g + 1)) x' (x' ^ (n `Haskell.div` 2) /= one)

inv :: Integer -> Natural -> Natural
inv a p =
  Haskell.fromIntegral $
    snd (egcd (a, 1) (fromConstant p, 0)) `Haskell.mod` fromConstant p
 where
  egcd (x, y) (0, _) = (x, y)
  egcd (x, y) (x', y') = egcd (x', y') (x - q * x', y - q * y')
   where
    q = x `div` x'

instance Prime p => BinaryExpansion (Zp p) where
  type Bits (Zp p) = [Zp p]
  binaryExpansion = Haskell.map (Zp . fromConstant) . binaryExpansion . fromZp

instance KnownNat p => Haskell.Num (Zp p) where
  fromInteger = toZp
  (+) = (+)
  (-) = (-)
  (*) = (*)
  negate = negate
  abs = id
  signum = const 1

instance Prime p => Haskell.Fractional (Zp p) where
  fromRational = Haskell.error "`fromRational` is not implemented for `Zp p`"
  recip = finv
  (/) = (//)

instance Haskell.Show (Zp p) where
  show (Zp a) = Haskell.show a

instance ToJSON (Zp p) where
  toJSON (Zp a) = toJSON a

instance FromJSON (Zp p) where
  parseJSON = Haskell.fmap Zp . parseJSON

instance KnownNat p => Binary (Zp p) where
  put (Zp x) = go x (wordCount @p)
   where
    go _ 0 = pure ()
    go n count =
      let (n', r) = n `Haskell.divMod` 256
       in putWord8 (Haskell.fromIntegral r) <> go n' (count -! 1)
  get = toZp <$> go (wordCount @p)
   where
    go 0 = pure zero
    go n = liftA2 combine getWord8 (go $ n -! 1) <|> pure zero
    combine r d = Haskell.fromIntegral r + 256 * d

wordCount :: forall p. KnownNat p => Natural
wordCount = Haskell.ceiling $ log2ceiling (value @p) % (8 :: Natural)

instance KnownNat p => Arbitrary (Zp p) where
  arbitrary = toZp <$> chooseInteger (0, Haskell.fromIntegral (value @p) - 1)

instance KnownNat p => Random (Zp p)

instance KnownNat p => Uniform (Zp p) where
  uniformM = fmap Zp . uniformRM (0, Haskell.fromIntegral (value @p) - 1)

instance KnownNat p => UniformRange (Zp p) where
  uniformRM (Zp a, Zp b)
    | a Haskell.<= b = fmap Zp . uniformRM (a, b)
    | Haskell.otherwise = fmap fromConstant . uniformRM (a, b + integral @p)

-- | Exponentiation by an element of a finite field is well-defined (and lawful)
-- if and only if the base is a finite multiplicative group of a matching order.
--
-- Note that left distributivity is satisfied, meaning
-- @a ^ (m + n) = (a ^ m) * (a ^ n)@.
instance (MultiplicativeGroup a, Order a ~ p) => Exponent a (Zp p) where
  a ^ n = a ^ fromZp n

----------------------------- Field Extensions --------------------------------

class (Ring poly, UnivariateFieldPolynomial f poly) => IrreduciblePoly poly f (e :: Symbol) | e -> f, e -> poly where
  irreduciblePoly :: poly

data Ext2 f (e :: Symbol) = Ext2 f f
  deriving (FromJSON, Generic, Haskell.Eq, Haskell.Show, ToJSON)

instance Haskell.Ord f => Haskell.Ord (Ext2 f e) where
  Ext2 a b <= Ext2 c d = [b, a] Haskell.<= ([d, c] :: [f])

instance (KnownNat (Order (Ext2 f e)), KnownNat (NumberOfBits (Ext2 f e))) => Finite (Ext2 f e) where
  type Order (Ext2 f e) = Order f ^ 2

instance {-# OVERLAPPING #-} FromConstant (Ext2 f e) (Ext2 f e)

instance Field f => AdditiveSemigroup (Ext2 f e) where
  Ext2 a b + Ext2 c d = Ext2 (a + c) (b + d)

instance Scale c f => Scale c (Ext2 f e) where
  scale c (Ext2 a b) = Ext2 (scale c a) (scale c b)

instance Zero f => Zero (Ext2 f e) where
  zero = Ext2 zero zero

instance Field f => AdditiveMonoid (Ext2 f e)

instance Field f => AdditiveGroup (Ext2 f e) where
  negate (Ext2 a b) = Ext2 (negate a) (negate b)
  Ext2 a b - Ext2 c d = Ext2 (a - c) (b - d)

instance {-# OVERLAPPING #-} (Field f, Eq f, IrreduciblePoly poly f e) => Scale (Ext2 f e) (Ext2 f e)

instance (Field f, Eq f, IrreduciblePoly poly f e) => MultiplicativeSemigroup (Ext2 f e) where
  Ext2 a b * Ext2 c d = fromConstant @poly (toPoly @f @poly [a, b] * toPoly @f @poly [c, d])

instance MultiplicativeMonoid (Ext2 f e) => Exponent (Ext2 f e) Natural where
  (^) = natPow

instance (Field f, Eq f, IrreduciblePoly poly f e) => MultiplicativeMonoid (Ext2 f e) where
  one = Ext2 one zero

instance Field (Ext2 f e) => Exponent (Ext2 f e) Integer where
  (^) = intPowF

instance Eq field => Eq (Ext2 field i)

instance
  ( Field f
  , Eq f
  , Conditional (BooleanOf f) (Ext2 f e)
  , IrreduciblePoly poly f e
  )
  => Field (Ext2 f e)
  where
  finv (Ext2 a b) =
    let (g, s) = eea (toPoly [a, b]) (irreduciblePoly @poly @f @e)
     in case fromPoly $ scaleP (one // lt g) 0 s of
          [] -> Ext2 zero zero
          [x] -> Ext2 x zero
          v -> Ext2 (v V.! 0) (v V.! 1)

  rootOfUnity n = (`Ext2` zero) <$> rootOfUnity n

instance (FromConstant c poly, IrreduciblePoly poly f e) => FromConstant c (Ext2 f e) where
  fromConstant p = case fromPoly . snd $ qr (fromConstant p) (irreduciblePoly @poly @f @e) of
    [] -> zero
    [x] -> Ext2 x zero
    v -> Ext2 (v V.! 0) (v V.! 1)

instance (Field f, Eq f, IrreduciblePoly poly f e) => Semiring (Ext2 f e)

instance (Field f, Eq f, IrreduciblePoly poly f e) => Ring (Ext2 f e)

instance Binary f => Binary (Ext2 f e) where
  put (Ext2 a b) = put a <> put b
  get = Ext2 <$> get <*> get

instance (Field f, Eq f, IrreduciblePoly poly f e, Arbitrary f) => Arbitrary (Ext2 f e) where
  arbitrary = Ext2 <$> arbitrary <*> arbitrary

data Ext3 f (e :: Symbol) = Ext3 f f f
  deriving (FromJSON, Generic, Haskell.Eq, Haskell.Show, ToJSON)

instance Haskell.Ord f => Haskell.Ord (Ext3 f e) where
  Ext3 a b c <= Ext3 d e f = [c, b, a] Haskell.<= ([f, e, d] :: [f])

instance (KnownNat (Order (Ext3 f e)), KnownNat (NumberOfBits (Ext3 f e))) => Finite (Ext3 f e) where
  type Order (Ext3 f e) = Order f ^ 3

instance {-# OVERLAPPING #-} FromConstant (Ext3 f e) (Ext3 f e)

instance Field f => AdditiveSemigroup (Ext3 f e) where
  Ext3 a b c + Ext3 d e f = Ext3 (a + d) (b + e) (c + f)

instance Scale c f => Scale c (Ext3 f e) where
  scale c (Ext3 d e f) = Ext3 (scale c d) (scale c e) (scale c f)

instance Zero f => Zero (Ext3 f e) where
  zero = Ext3 zero zero zero

instance Field f => AdditiveMonoid (Ext3 f e)

instance Field f => AdditiveGroup (Ext3 f e) where
  negate (Ext3 a b c) = Ext3 (negate a) (negate b) (negate c)
  Ext3 a b c - Ext3 d e f = Ext3 (a - d) (b - e) (c - f)

instance {-# OVERLAPPING #-} (Field f, Eq f, IrreduciblePoly poly f e) => Scale (Ext3 f e) (Ext3 f e)

instance (Field f, Eq f, IrreduciblePoly poly f e) => MultiplicativeSemigroup (Ext3 f e) where
  Ext3 a b c * Ext3 d e f = fromConstant @poly (toPoly [a, b, c] * toPoly [d, e, f])

instance MultiplicativeMonoid (Ext3 f e) => Exponent (Ext3 f e) Natural where
  (^) = natPow

instance (Field f, Eq f, IrreduciblePoly poly f e) => MultiplicativeMonoid (Ext3 f e) where
  one = Ext3 one zero zero

instance Field (Ext3 f e) => Exponent (Ext3 f e) Integer where
  (^) = intPowF

instance Eq field => Eq (Ext3 field i)

instance
  ( Field f
  , Eq f
  , Conditional (BooleanOf f) (Ext3 f e)
  , IrreduciblePoly poly f e
  )
  => Field (Ext3 f e)
  where
  finv (Ext3 a b c) =
    let (g, s) = eea (toPoly [a, b, c]) (irreduciblePoly @poly @f @e)
     in case fromPoly $ scaleP (one // lt g) 0 s of
          [] -> Ext3 zero zero zero
          [x] -> Ext3 x zero zero
          [x, y] -> Ext3 x y zero
          v -> Ext3 (v V.! 0) (v V.! 1) (v V.! 2)

  rootOfUnity n = (\r -> Ext3 r zero zero) <$> rootOfUnity n

instance (FromConstant c poly, IrreduciblePoly poly f e) => FromConstant c (Ext3 f e) where
  fromConstant p = case fromPoly . snd $ qr (fromConstant p) (irreduciblePoly @poly @f @e) of
    [] -> zero
    [x] -> Ext3 x zero zero
    [x, y] -> Ext3 x y zero
    v -> Ext3 (v V.! 0) (v V.! 1) (v V.! 2)

instance (Field f, Eq f, IrreduciblePoly poly f e) => Semiring (Ext3 f e)

instance (Field f, Eq f, IrreduciblePoly poly f e) => Ring (Ext3 f e)

instance Binary f => Binary (Ext3 f e) where
  put (Ext3 a b c) = put a <> put b <> put c
  get = Ext3 <$> get <*> get <*> get

instance (Field f, Eq f, IrreduciblePoly poly f e, Arbitrary f) => Arbitrary (Ext3 f e) where
  arbitrary = Ext3 <$> arbitrary <*> arbitrary <*> arbitrary
