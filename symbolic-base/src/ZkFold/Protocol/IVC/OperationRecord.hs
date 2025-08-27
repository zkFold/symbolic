module ZkFold.Protocol.IVC.OperationRecord where

import Data.Bifunctor (Bifunctor (..))
import Data.Bool (Bool)
import Data.Eq (Eq (..))
import Data.Functor (Functor, fmap, (<$>))
import Data.List ((++))
import Data.Tuple (snd)
import Prelude (Integer)

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import ZkFold.Data.Bool
import ZkFold.Protocol.IVC.AccumulatorScheme (AdditiveAction (..))

data Operands s c = Addends c c | Scaling s c deriving Functor

addends :: AdditiveSemigroup c => c -> c -> (Operands s c, c)
addends a b = (Addends a b, a + b)

scaling :: Scale s c => s -> c -> (Operands s c, c)
scaling k c = (Scaling k c, scale k c)

compute :: (AdditiveSemigroup c, Scale s c) => Operands s c -> c
compute = \case
  Addends c d -> c + d
  Scaling k c -> k *. c

validate :: (AdditiveSemigroup c, Scale s c, Eq c) => (Operands s c, c) -> Bool
validate (compute -> c, d) = c == d

instance Bifunctor Operands where
  first f = \case
    Addends a b -> Addends a b
    Scaling k c -> Scaling (f k) c
  second = fmap

data OperationRecord s c = OperationRecord
  { opValue :: c
  , opLog :: [(Operands s c, c)]
  }
  deriving Functor

record :: (Operands s c, c) -> [(Operands s c, c)] -> OperationRecord s c
record res log = OperationRecord (snd res) (res : log)

instance Bifunctor OperationRecord where
  first f OperationRecord {..} =
    OperationRecord
      { opValue = opValue
      , opLog = first (first f) <$> opLog
      }
  second = fmap

instance Zero c => Zero (OperationRecord s c) where
  zero = OperationRecord zero []

instance AdditiveSemigroup c => AdditiveSemigroup (OperationRecord s c) where
  OperationRecord a l + OperationRecord b m = addends a b `record` (l ++ m)

instance
  (Semiring s, AdditiveMonoid c, Scale s c)
  => AdditiveMonoid (OperationRecord s c)

instance
  (Ring s, AdditiveMonoid c, Scale s c)
  => AdditiveGroup (OperationRecord s c)
  where
  negate = scale (-1 :: Integer)

instance AdditiveGroup c => AdditiveAction c (OperationRecord s c) where
  OperationRecord a l .+ c = addends a c `record` l

instance (FromConstant k s, Scale s c) => Scale k (OperationRecord s c) where
  scale (fromConstant -> k) OperationRecord {..} =
    scaling k opValue `record` opLog

instance (FiniteField s, Scale s c, CyclicGroup c) => CyclicGroup (OperationRecord s c) where
  type ScalarFieldOf (OperationRecord s c) = s
  pointGen = zero .+ pointGen @c

instance (AdditiveSemigroup c, Eq c, Scale s c) => Eq (OperationRecord s c) where
  OperationRecord a l == OperationRecord b m =
    a == b && all validate l && all validate m
