{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Vec where

import Control.DeepSeq (NFData, NFData1)
import Data.Functor.Classes (Eq1)
import Data.Functor.Rep
import Data.Traversable (Traversable (..))
import GHC.Generics (Generic)
import GHC.Num (Natural)
import Prelude (Integer, ($), (.))
import qualified Prelude as Haskell

import ZkFold.Algebra.Class
import ZkFold.Data.HFunctor.Classes (HEq, HNFData)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators (mzipWithMRep)
import ZkFold.Symbolic.Data.Conditional (Conditional)
import ZkFold.Symbolic.Data.Eq (Eq)
import ZkFold.Symbolic.Data.Input
import ZkFold.Symbolic.MonadCircuit

newtype Vec f c = Vec {runVec :: c f}

deriving newtype instance
  (LayoutFunctor f, Symbolic c)
  => SymbolicData (Vec f c)

deriving newtype instance
  (LayoutFunctor f, Symbolic c)
  => SymbolicInput (Vec f c)

deriving instance Generic (Vec f c)

deriving instance (HNFData c, NFData1 f) => NFData (Vec f c)

deriving instance (Eq1 f, HEq c) => Haskell.Eq (Vec f c)

deriving newtype instance (LayoutFunctor f, Symbolic c) => Eq (Vec f c)

deriving newtype instance
  (LayoutFunctor f, Symbolic c)
  => Conditional (Bool c) (Vec f c)

instance
  {-# INCOHERENT #-}
  (FromConstant k (BaseField c), Representable f, Symbolic c)
  => FromConstant k (Vec f c)
  where
  fromConstant = Vec . embed . tabulate . fromConstant

instance
  (Representable f, Symbolic c, Traversable f)
  => MultiplicativeSemigroup (Vec f c)
  where
  Vec v1 * Vec v2 =
    Vec $
      fromCircuit2F v1 v2 $
        mzipWithMRep
          ( \i j ->
              newAssigned $ ($ i) * ($ j)
          )

instance (Representable f, Symbolic c, Traversable f) => Scale Natural (Vec f c)

instance (Representable f, Symbolic c, Traversable f) => Scale Integer (Vec f c)

instance (Symbolic c, Traversable f) => Exponent (Vec f c) Natural where
  Vec v ^ n = Vec $ fromCircuitF v $ traverse (\i -> newAssigned $ ($ i) ^ n)

instance
  (Representable f, Symbolic c, Traversable f)
  => AdditiveSemigroup (Vec f c)
  where
  Vec v1 + Vec v2 =
    Vec $
      fromCircuit2F v1 v2 $
        mzipWithMRep
          ( \i j ->
              newAssigned $ ($ i) + ($ j)
          )

instance
  (Representable f, Symbolic c, Traversable f)
  => MultiplicativeMonoid (Vec f c)
  where
  one = fromConstant (1 :: Natural)

instance
  (Representable f, Symbolic c, Traversable f)
  => AdditiveMonoid (Vec f c)
  where
  zero = fromConstant (0 :: Natural)

instance
  (Representable f, Symbolic c, Traversable f)
  => AdditiveGroup (Vec f c)
  where
  negate (Vec v) =
    Vec $
      fromCircuitF v $
        traverse
          ( \i ->
              newAssigned $ negate ($ i)
          )

instance (Representable f, Symbolic c, Traversable f) => Semiring (Vec f c)

instance (Representable f, Symbolic c, Traversable f) => Ring (Vec f c)
