{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Vec where

import           Control.DeepSeq                  (NFData, NFData1)
import           Data.Functor.Classes             (Eq1)
import           Data.Functor.Rep
import           Data.Traversable                 (Traversable (..))
import           GHC.Generics                     (Generic)
import           GHC.Num                          (Natural)
import           Prelude                          (Integer, ($), (.))
import qualified Prelude                          as Haskell

import           ZkFold.Algebra.Class
import           ZkFold.Data.HFunctor.Classes     (HEq, HNFData)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool        (true)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators (mzipWithMRep)
import           ZkFold.Symbolic.Data.Input
import           ZkFold.Symbolic.MonadCircuit

newtype Vec f c = Vec { runVec :: c f }

instance SymbolicData (Vec f) where
    type Layout (Vec f) a = f
    toContext = runVec
    fromContext = Vec

instance SymbolicInput (Vec f) where
    isValid _ = true

deriving instance Generic (Vec f c)
deriving instance (HNFData c, NFData1 f) => NFData (Vec f c)
deriving instance (HEq c, Eq1 f) => Haskell.Eq (Vec f c)

instance {-# INCOHERENT #-} (Symbolic c, Representable f, FromConstant k (BaseField c), LayoutFunctor f) =>
  FromConstant k (Vec f c) where
    fromConstant = fromContext . embed . tabulate . fromConstant

instance (Symbolic c, LayoutFunctor f) =>
  MultiplicativeSemigroup (Vec f c) where
    v1 * v2 = fromContext $ fromCircuit2F (toContext v1) (toContext v2) $ mzipWithMRep (\i j ->
      newAssigned $ ($ i) * ($ j))

instance (Symbolic c, LayoutFunctor f) => Scale Natural (Vec f c)
instance (Symbolic c, LayoutFunctor f) => Scale Integer (Vec f c)
instance (Symbolic c, LayoutFunctor f) => Exponent (Vec f c) Natural where
    v ^ n = fromContext $ fromCircuitF (toContext v) $ traverse (\i -> newAssigned $ ($ i) ^ n)

instance (Symbolic c, LayoutFunctor f) =>
  AdditiveSemigroup (Vec f c) where
    v1 + v2 = fromContext $ fromCircuit2F (toContext v1) (toContext v2) $ mzipWithMRep (\i j ->
      newAssigned $ ($ i) + ($ j))

instance (Symbolic c, LayoutFunctor f) =>
  MultiplicativeMonoid (Vec f c) where
    one = fromConstant (1 :: Natural)

instance (Symbolic c, LayoutFunctor f) =>
  AdditiveMonoid (Vec f c) where
    zero = fromConstant (0 :: Natural)

instance (Symbolic c, LayoutFunctor f) =>
  AdditiveGroup (Vec f c) where
    negate v = fromContext $ fromCircuitF (toContext v) $ traverse (\i ->
      newAssigned $ negate ($ i))

instance (Symbolic c, LayoutFunctor f) => Semiring (Vec f c)

instance (Symbolic c, LayoutFunctor f) => Ring (Vec f c)
