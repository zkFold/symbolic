{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Vec where

import Control.DeepSeq (NFData, NFData1)
import Data.Constraint (Dict (..))
import Data.Functor (fmap)
import Data.Functor.Classes (Eq1)
import Data.Functor.Rep
import Data.Kind (Type)
import Data.Traversable (Traversable (..))
import Data.Tuple (fst)
import qualified GHC.Generics as G
import GHC.Num (Natural)
import Prelude (Integer, ($), (.))
import qualified Prelude as Haskell

import ZkFold.Algebra.Class
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.HFunctor.Classes (HEq, HNFData)
import qualified ZkFold.Symbolic.Algorithm.Interpolation as I
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators (mzipWithMRep)
import ZkFold.Symbolic.MonadCircuit

newtype Vec (f :: Type -> Type) c = Vec {runVec :: c f}
  deriving G.Generic

deriving instance (HNFData c, NFData1 f) => NFData (Vec f c)

deriving instance (HEq c, Eq1 f) => Haskell.Eq (Vec f c)

deriving newtype instance Eq (c f) => Eq (Vec f c)

instance LayoutFunctor f => SymbolicData (Vec f) where
  type Layout (Vec f) n = f
  type Payload (Vec f) n = G.U1
  type HasRep (Vec f) _ = Representable f

  dataFunctor _ = Dict
  hasRep _ = Dict
  arithmetize = runVec
  payload _ = G.U1
  interpolate bs = Vec . I.interpolate (fmap (fmap runVec) bs)
  restore = Vec . fst

instance
  (Symbolic c, Representable f, FromConstant k (BaseField c))
  => FromConstant k (Vec f c)
  where
  fromConstant = Vec . embed . tabulate . fromConstant

instance {-# OVERLAPPING #-} FromConstant (Vec f c) (Vec f c)

instance
  (Symbolic c, Traversable f, Representable f)
  => MultiplicativeSemigroup (Vec f c)
  where
  Vec v1 * Vec v2 =
    Vec $
      fromCircuit2F v1 v2 $
        mzipWithMRep
          ( \i j ->
              newAssigned $ ($ i) * ($ j)
          )

instance (Symbolic c, Traversable f, Representable f) => Scale Natural (Vec f c)

instance (Symbolic c, Traversable f, Representable f) => Scale Integer (Vec f c)

instance (Symbolic c, Traversable f) => Exponent (Vec f c) Natural where
  Vec v ^ n = Vec $ fromCircuitF v $ traverse (\i -> newAssigned $ ($ i) ^ n)

instance
  (Symbolic c, Traversable f, Representable f)
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
  (Symbolic c, Traversable f, Representable f)
  => MultiplicativeMonoid (Vec f c)
  where
  one = fromConstant (1 :: Natural)

instance (Symbolic c, Representable f) => Zero (Vec f c) where
  zero = fromConstant (0 :: Natural)

instance (Symbolic c, Traversable f, Representable f) => AdditiveMonoid (Vec f c)

instance
  (Symbolic c, Traversable f, Representable f)
  => AdditiveGroup (Vec f c)
  where
  negate (Vec v) =
    Vec $
      fromCircuitF v $
        traverse
          ( \i ->
              newAssigned $ negate ($ i)
          )

instance (Symbolic c, Traversable f, Representable f) => Semiring (Vec f c)

instance (Symbolic c, Traversable f, Representable f) => Ring (Vec f c)
