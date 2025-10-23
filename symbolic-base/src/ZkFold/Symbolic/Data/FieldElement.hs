{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Symbolic.Data.FieldElement where

import Control.DeepSeq (NFData)
import Data.Function (($), (.))
import Data.Functor (Functor, fmap, (<$>))
import GHC.Generics (Generic, Par1 (..))
import Test.QuickCheck (Arbitrary (..))
import Prelude (Integer)
import qualified Prelude as Haskell

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.Vector (Vector, unsafeToVector)
import ZkFold.Symbolic.Data.Input
import ZkFold.Symbolic.Data.Ord
import ZkFold.Symbolic.Data.Vec (Vec (..))
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Class (Symbolic)

newtype FieldElement c = FieldElement { fromFieldElement :: c }
  deriving stock (Generic, Haskell.Show, Haskell.Eq, Haskell.Ord)
  deriving anyclass NFData
  deriving (SymbolicData, SymbolicInput) via (Vec Par1)
  deriving ( Eq, Ord, MultiplicativeSemigroup, MultiplicativeMonoid
           , AdditiveSemigroup, Zero, Scale Natural, AdditiveMonoid
           , Scale Integer, AdditiveGroup, Semiring, Ring) via (Vec Par1 c)

fieldElements :: Functor f => f c -> f (FieldElement c)
fieldElements = fmap FieldElement

instance (Symbolic c, FromConstant k c) => FromConstant k (FieldElement c) where
  fromConstant = FieldElement . fromConstant

instance ToConstant (FieldElement a) where
  type Const (FieldElement a) = a
  toConstant = fromFieldElement

instance Symbolic c => Exponent (FieldElement c) Natural where
  (^) = natPow

instance Symbolic c => Exponent (FieldElement c) Integer where
  (^) = intPowF

instance {-# OVERLAPPING #-} FromConstant (FieldElement c) (FieldElement c)

instance {-# OVERLAPPING #-} Symbolic c => Scale (FieldElement c) (FieldElement c)

instance Symbolic c => Field (FieldElement c) where
  finv (FieldElement _x) =
    FieldElement $ Haskell.error "TODO"
      -- symbolicF x (\(Par1 v) -> Par1 (finv v)) $
      --  fmap snd . runInvert

instance
  ( KnownNat (Order (FieldElement c))
  , KnownNat (NumberOfBits (FieldElement c))
  )
  => Finite (FieldElement c)
  where
  type Order (FieldElement c) = Order c

instance Symbolic c => BinaryExpansion (FieldElement c) where
  type Bits (FieldElement c) = Vector (NumberOfBits c) c
  binaryExpansion (FieldElement _c) =
    unsafeToVector $ Haskell.error "TODO"
      -- symbolicF
      --   c
      --   (padBits n . fmap fromConstant . binaryExpansion . toConstant . unPar1)
      --   (expansion n . unPar1)
   where
    _n = numberOfBits @c
  fromBinary _bits =
    FieldElement $ Haskell.error "TODO"
      -- symbolicF bits (Par1 . foldr (\x y -> x + y + y) zero) $
      --  fmap Par1 . horner . fromVector

instance Arbitrary c => Arbitrary (FieldElement c) where
  arbitrary = FieldElement <$> arbitrary
