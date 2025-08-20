{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.IVC.ForeignField where

import qualified Data.Eq as Haskell
import GHC.Generics (Generic)
import Prelude (Integer, Num (fromInteger))

import ZkFold.Algebra.Class
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number (KnownNat, Natural, Prime, value)
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Symbolic.MonadCircuit (IntegralOf, ResidueField, fromIntegral, toIntegral)

newtype ForeignField q i = ForeignField {foreignField :: i}
  deriving (Generic, Haskell.Eq)

instance (KnownNat q, Euclidean i) => FromConstant Natural (ForeignField q i) where
  fromConstant x = ForeignField (fromConstant x `mod` fromConstant (value @q))

instance (KnownNat q, Euclidean i) => FromConstant Integer (ForeignField q i) where
  fromConstant x = ForeignField (fromConstant x `mod` fromConstant (value @q))

instance Eq i => Eq (ForeignField q i) where
  type BooleanOf (ForeignField q i) = BooleanOf i
  ForeignField f == ForeignField g = f == g
  ForeignField f /= ForeignField g = f /= g

deriving instance {-# INCOHERENT #-} Conditional b i => Conditional b (ForeignField q i)

instance (KnownNat q, Euclidean i) => Scale Natural (ForeignField q i) where
  scale k (ForeignField f) = ForeignField (scale k f `mod` fromConstant (value @q))

instance (KnownNat q, Euclidean i) => Scale Integer (ForeignField q i) where
  scale k (ForeignField f) = ForeignField (scale k f `mod` fromConstant (value @q))

instance (KnownNat q, Euclidean i) => Exponent (ForeignField q i) Natural where
  (^) = natPow

instance (KnownNat q, Euclidean i) => AdditiveSemigroup (ForeignField q i) where
  ForeignField f + ForeignField g = ForeignField ((f + g) `mod` fromConstant (value @q))

instance Zero i => Zero (ForeignField q i) where
  zero = ForeignField zero

instance (KnownNat q, Euclidean i) => AdditiveMonoid (ForeignField q i)

instance (KnownNat q, Euclidean i) => AdditiveGroup (ForeignField q i) where
  negate (ForeignField f) = ForeignField (negate f `mod` fromConstant (value @q))
  ForeignField f - ForeignField g = ForeignField ((f - g) `mod` fromConstant (value @q))

instance (KnownNat q, Euclidean i) => MultiplicativeSemigroup (ForeignField q i) where
  ForeignField f * ForeignField g = ForeignField ((f * g) `mod` fromConstant (value @q))

instance (KnownNat q, Euclidean i) => MultiplicativeMonoid (ForeignField q i) where
  one = ForeignField one

instance (KnownNat q, Euclidean i) => Semiring (ForeignField q i)

instance (KnownNat q, Euclidean i) => Ring (ForeignField q i)

instance (KnownNat q, Euclidean i) => SemiEuclidean (ForeignField q i) where
  ForeignField f `div` ForeignField g = ForeignField ((f `div` g) `mod` fromConstant (value @q))
  ForeignField f `mod` ForeignField g = ForeignField ((f `mod` g) `mod` fromConstant (value @q))

instance (KnownNat q, Euclidean i) => Euclidean (ForeignField q i) where
  ForeignField f `gcd` ForeignField g = ForeignField ((f `gcd` g) `mod` fromConstant (value @q))
  ForeignField f `bezoutL` ForeignField g = ForeignField ((f `bezoutL` g) `mod` fromConstant (value @q))
  ForeignField f `bezoutR` ForeignField g = ForeignField ((f `bezoutR` g) `mod` fromConstant (value @q))

instance (Prime q, Euclidean i) => Exponent (ForeignField q i) Integer where
  f ^ p = f ^ fromInteger @Natural (p `mod` (fromConstant (value @q) - 1))

instance (Prime q, Eq i, Euclidean i, Conditional (BooleanOf i) i) => Field (ForeignField q i) where
  finv f = f ^ (value @q -! 2)

instance (KnownNat q, KnownNat (NumberOfBits (Zp q))) => Finite (ForeignField q i) where
  type Order (ForeignField q i) = q

instance
  ( Prime q
  , KnownNat (NumberOfBits (Zp q))
  , Eq i
  , Euclidean i
  , Conditional (BooleanOf i) (BooleanOf i)
  , Conditional (BooleanOf i) i
  )
  => ResidueField (ForeignField q i)
  where
  type IntegralOf (ForeignField q i) = i
  fromIntegral i = ForeignField (i `mod` fromConstant (value @q))
  toIntegral (ForeignField i) = i
