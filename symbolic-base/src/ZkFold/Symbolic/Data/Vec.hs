{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module ZkFold.Symbolic.Data.Vec where


import           Control.DeepSeq                  (NFData)
import           Data.Functor.Rep
import           Data.Semialign                   (Zip)
import           Data.Traversable                 (Traversable (..))
import           Data.Type.Equality               (type (~))
import           GHC.Generics                     (Generic)
import           GHC.Num                          (Natural)
import           Prelude                          (Integer, ($), (.))
import qualified Prelude                          as Haskell

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Data.Utils           (zipWithM)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool        (Bool)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Conditional (Conditional)
import           ZkFold.Symbolic.Data.Eq          (Eq)
import           ZkFold.Symbolic.Data.Input
import           ZkFold.Symbolic.MonadCircuit


newtype Vec f c = Vec { runVec :: c f }

deriving newtype instance (Symbolic c, LayoutFunctor f) => SymbolicData (Vec f c)
deriving newtype instance (Symbolic c, LayoutFunctor f) => SymbolicInput (Vec f c)
deriving instance Generic (Vec f c)
deriving instance (NFData (c f)) => NFData (Vec f c)
deriving instance (Haskell.Eq (c f)) => Haskell.Eq (Vec f c)

deriving newtype instance (Symbolic c, LayoutFunctor f) => Eq (Vec f c)
deriving newtype instance (Symbolic c, LayoutFunctor f) => Conditional (Bool c) (Vec f c)

instance (Symbolic c, Representable f) => FromConstant Natural (Vec f c) where
    fromConstant = Vec . embed . tabulate . fromConstant
instance (Symbolic c, Representable f) => FromConstant Integer (Vec f c) where
    fromConstant = Vec . embed . tabulate . fromConstant

instance {-# INCOHERENT #-} (Symbolic c) => FromConstant (Vec f c) (Vec f c)

instance {-# INCOHERENT #-} (Symbolic c, Representable f, a ~ BaseField c) => FromConstant a (Vec f c) where
    fromConstant = Vec . embed . tabulate . fromConstant

instance (Symbolic c, Traversable f, Zip f) => MultiplicativeSemigroup (Vec f c) where
    (Vec v1) * (Vec v2) = Vec $ fromCircuit2F v1 v2 $ zipWithM  (\i j -> newAssigned $ ($ i) * ($ j))

instance (Symbolic c, Traversable f, Representable f, Zip f) => Scale Natural (Vec f c)
instance (Symbolic c, Traversable f, Representable f, Zip f) => Scale Integer (Vec f c)
instance (Symbolic c, Traversable f) => Exponent (Vec f c) Natural where
    (Vec v) ^ n = Vec $ fromCircuitF v $ traverse (\i -> newAssigned $ ($ i) ^ n)


instance (Symbolic c, Traversable f, Zip f) => AdditiveSemigroup (Vec f c) where
    (Vec v1) + (Vec v2) = Vec $ fromCircuit2F v1 v2 $ zipWithM  (\i j -> newAssigned $ ($ i) + ($ j))

instance (Symbolic c, Traversable f, Zip f, Representable f) => MultiplicativeMonoid (Vec f c) where
    one = fromConstant (1 :: Natural)

instance (Symbolic c, Traversable f, Zip f, Representable f) => AdditiveMonoid (Vec f c) where
    zero = fromConstant (0 :: Natural)


instance (Symbolic c, Traversable f, Zip f, Representable f) => AdditiveGroup (Vec f c) where
    negate (Vec v) = Vec $ fromCircuitF v $ traverse (\i -> newAssigned $ negate ($ i) )

instance (Symbolic c, Traversable f, Zip f, Representable f) => Semiring (Vec f c)

instance (Symbolic c, Traversable f, Zip f, Representable f) => Ring (Vec f c)
