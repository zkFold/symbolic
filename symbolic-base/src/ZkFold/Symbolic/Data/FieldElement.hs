{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.FieldElement where

import           Control.DeepSeq                  (NFData)
import           Data.Foldable                    (foldr)
import           Data.Function                    (($), (.))
import           Data.Functor                     (fmap, (<$>))
import           Data.Tuple                       (snd)
import           GHC.Generics                     (Generic, Par1 (..))
import           Prelude                          (Integer)
import qualified Prelude                          as Haskell

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Data.HFunctor        (hmap)
import           ZkFold.Base.Data.Vector          (Vector, fromVector, unsafeToVector)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool        (Bool, BoolType (true))
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators (expansion, horner, runInvert)
import           ZkFold.Symbolic.Data.Conditional (Conditional)
import           ZkFold.Symbolic.Data.Eq          (Eq)
import           ZkFold.Symbolic.Data.Input
import           ZkFold.Symbolic.Data.Ord
import           ZkFold.Symbolic.Interpreter      (Interpreter (..))
import           ZkFold.Symbolic.MonadCircuit     (newAssigned)

newtype FieldElement c = FieldElement { fromFieldElement :: c Par1 }
    deriving Generic

deriving stock instance Haskell.Show (c Par1) => Haskell.Show (FieldElement c)

deriving stock instance Haskell.Eq (c Par1) => Haskell.Eq (FieldElement c)

deriving stock instance Haskell.Ord (c Par1) => Haskell.Ord (FieldElement c)

deriving newtype instance NFData (c Par1) => NFData (FieldElement c)

deriving newtype instance Symbolic c => SymbolicData (FieldElement c)

deriving newtype instance Symbolic c => Conditional (Bool c) (FieldElement c)

deriving newtype instance Symbolic c => Eq (FieldElement c)

deriving newtype instance Symbolic c => Ord (FieldElement c)

instance {-# INCOHERENT #-} (Symbolic c, FromConstant k (BaseField c)) => FromConstant k (FieldElement c) where
  fromConstant = FieldElement . embed . Par1 . fromConstant

instance ToConstant (FieldElement (Interpreter a)) where
  type Const (FieldElement (Interpreter a)) = a
  toConstant (FieldElement (Interpreter (Par1 x))) = x

instance Symbolic c => Exponent (FieldElement c) Natural where
  (^) = natPow

instance Symbolic c => Exponent (FieldElement c) Integer where
  (^) = intPowF

instance (Symbolic c, Scale k (BaseField c)) => Scale k (FieldElement c) where
  scale k (FieldElement c) = FieldElement $ fromCircuitF c $ \(Par1 i) ->
    Par1 <$> newAssigned (\x -> fromConstant (scale k one :: BaseField c) * x i)

instance {-# OVERLAPPING #-} FromConstant (FieldElement c) (FieldElement c)

instance {-# OVERLAPPING #-} Symbolic c => Scale (FieldElement c) (FieldElement c)

instance Symbolic c => MultiplicativeSemigroup (FieldElement c) where
  FieldElement x * FieldElement y = FieldElement $ fromCircuit2F x y
    $ \(Par1 i) (Par1 j) -> Par1 <$> newAssigned (\w -> w i * w j)

instance Symbolic c => MultiplicativeMonoid (FieldElement c) where
  one = FieldElement $ embed (Par1 one)

instance Symbolic c => AdditiveSemigroup (FieldElement c) where
  FieldElement x + FieldElement y = FieldElement $ fromCircuit2F x y
    $ \(Par1 i) (Par1 j) -> Par1 <$> newAssigned (\w -> w i + w j)

instance Symbolic c => AdditiveMonoid (FieldElement c) where
  zero = FieldElement $ embed (Par1 zero)

instance Symbolic c => AdditiveGroup (FieldElement c) where
  negate (FieldElement x) = FieldElement $ fromCircuitF x $ \(Par1 i) ->
    Par1 <$> newAssigned (\w -> negate (w i))

  FieldElement x - FieldElement y = FieldElement $ fromCircuit2F x y
    $ \(Par1 i) (Par1 j) -> Par1 <$> newAssigned (\w -> w i - w j)

instance Symbolic c => Semiring (FieldElement c)

instance Symbolic c => Ring (FieldElement c)

instance Symbolic c => Field (FieldElement c) where
  finv (FieldElement x) =
    FieldElement $ symbolicF x (\(Par1 v) -> Par1 (finv v))
      $ fmap snd . runInvert

instance
    ( KnownNat (Order (FieldElement c))
    , KnownNat (NumberOfBits (FieldElement c))) => Finite (FieldElement c) where
  type Order (FieldElement c) = Order (BaseField c)

instance Symbolic c => BinaryExpansion (FieldElement c) where
  type Bits (FieldElement c) = c (Vector (NumberOfBits (BaseField c)))
  binaryExpansion (FieldElement c) = hmap unsafeToVector $ symbolicF c
    (padBits n . fmap fromConstant . binaryExpansion . toConstant . unPar1)
    (expansion n . unPar1)
    where n = numberOfBits @(BaseField c)
  fromBinary bits =
    FieldElement $ symbolicF bits (Par1 . foldr (\x y -> x + y + y) zero)
      $ fmap Par1 . horner . fromVector

instance (Symbolic c) => SymbolicInput (FieldElement c) where
  isValid _ = true
