{-# LANGUAGE DerivingStrategies #-}

module ZkFold.Symbolic.Boot (Bool (..), FieldElement (..)) where

import qualified Data.Bool as Haskell
import Data.Function (($), (.))
import Data.Functor (Functor, fmap, (<$>))
import Data.Tuple (fst, snd)
import GHC.Generics (Generic1, type (:*:) (..))
import GHC.Integer (Integer)
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary)
import Text.Show (Show)

import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (Conditional (..), ifThenElse)
import ZkFold.Data.Bool (BoolType (..))
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.Product (toPair)
import ZkFold.Symbolic.Class (Symbolic, assigned, constrain, (=!=))

newtype FieldElement c = FieldElement {fromFieldElement :: c}
  deriving stock (Functor, Generic1, Show)
  deriving newtype (AdditiveMonoid, Arbitrary, MultiplicativeMonoid, Zero)

deriving newtype instance Finite c => Finite (FieldElement c)

instance FromConstant k c => FromConstant k (FieldElement c) where
  fromConstant = FieldElement . fromConstant

instance ToConstant (FieldElement a) where
  type Const (FieldElement a) = a
  toConstant = fromFieldElement

instance {-# OVERLAPPING #-} FromConstant (FieldElement c) (FieldElement c)

instance {-# OVERLAPPING #-} Symbolic c => Scale (FieldElement c) (FieldElement c)

instance Symbolic c => Scale Natural (FieldElement c) where
  scale k (FieldElement x) = FieldElement $ assigned (scale k ($ x))

instance Symbolic c => Scale Integer (FieldElement c) where
  scale k (FieldElement x) = FieldElement $ assigned (scale k ($ x))

instance Symbolic c => MultiplicativeSemigroup (FieldElement c) where
  FieldElement x * FieldElement y = FieldElement $ assigned (($ x) * ($ y))

instance Symbolic c => AdditiveSemigroup (FieldElement c) where
  FieldElement x + FieldElement y = FieldElement $ assigned (($ x) + ($ y))

instance Symbolic c => AdditiveGroup (FieldElement c) where
  negate (FieldElement x) = FieldElement $ assigned (negate ($ x))

instance Symbolic c => Semiring (FieldElement c)

instance Symbolic c => Ring (FieldElement c)

instance Symbolic c => Exponent (FieldElement c) Natural where
  (^) = natPow

-- TODO (Issue #18): hide this constructor
newtype Bool c = Bool {fromBool :: c} deriving (Functor, Generic1, Show)

bTrue :: Semiring c => Bool c
bTrue = Bool one

bFalse :: Zero c => Bool c
bFalse = Bool zero

bIf :: Symbolic c => Bool c -> c -> c -> c
bIf (Bool (FieldElement -> c)) (FieldElement -> t) (FieldElement -> f) =
  fromFieldElement (c * t + (one - c) * f)

instance Symbolic c => FromConstant Haskell.Bool (Bool c) where
  fromConstant b = ifThenElse b bTrue bFalse

instance {-# INCOHERENT #-} Symbolic c => Conditional (Bool c) (Bool c) where
  bool (Bool f) (Bool t) c = Bool (bIf c t f)

instance Symbolic c => BoolType (Bool c) where
  true = Bool one
  false = Bool zero
  not (Bool b) = Bool $ assigned (one - ($ b))
  Bool b && Bool c = Bool $ assigned (($ b) * ($ c))
  Bool b || Bool c = Bool $ assigned (($ b) + ($ c) - ($ b) * ($ c))
  Bool b `xor` Bool c =
    Bool $ assigned (($ b) + ($ c) - (one + one) * ($ b) * ($ c))

instance Symbolic c => Eq (Bool c) where
  type BooleanOf (Bool c) = Bool c
  b == b' = not (b `xor` b')

safeInverse :: Symbolic c => FieldElement c -> (Bool c, FieldElement c)
safeInverse (FieldElement x) =
  let isZero = ifThenElse (x == zero) one zero
      inverse = finv x
   in toPair $
        constrain (($ x) * ($ isZero) =!= zero)
          . constrain (($ x) * ($ inverse) + ($ isZero) =!= one)
          <$> (Bool isZero :*: FieldElement inverse)

instance Symbolic c => Eq (FieldElement c) where
  type BooleanOf (FieldElement c) = Bool c
  x == y = fst $ safeInverse (x - y)

instance {-# INCOHERENT #-} Symbolic c => Conditional (Bool c) (FieldElement c) where
  bool (FieldElement f) (FieldElement t) c = FieldElement (bIf c t f)

instance Symbolic c => Field (FieldElement c) where
  finv = snd . safeInverse
  rootOfUnity = fmap FieldElement . rootOfUnity

instance Symbolic c => Exponent (FieldElement c) Integer where
  (^) = intPowF
