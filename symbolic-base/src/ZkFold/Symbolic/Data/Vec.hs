{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Vec where

import Control.DeepSeq (NFData, NFData1)
import Data.Functor.Classes (Eq1)
import Data.Functor.Rep
import Data.Kind (Type)
import Data.Traversable (Traversable (..))
import qualified GHC.Generics as G
import GHC.Num (Natural)
import Test.QuickCheck (Arbitrary (arbitrary))
import Prelude (Integer, ($), (.))
import qualified Prelude as Haskell

import ZkFold.Algebra.Class
import ZkFold.Symbolic.Data.V2 (SymbolicData (..), RepFunctor)
import ZkFold.Symbolic.V2 (Symbolic)
import Data.Functor ((<$>), Functor)

newtype Vec (f :: Type -> Type) c = Vec {runVec :: f c}
  deriving G.Generic

deriving instance (NFData c, NFData1 f) => NFData (Vec f c)

deriving instance (Haskell.Eq c, Eq1 f) => Haskell.Eq (Vec f c)

instance Functor f => SymbolicData (Vec f) where
  type Layout (Vec f) _ = f
  type HasRep (Vec f) _ = RepFunctor f

  toLayout = runVec
  interpolate = Haskell.error "TODO"
  fromLayout = Vec

instance (Representable f, FromConstant k c) => FromConstant k (Vec f c) where
  fromConstant = Vec . tabulate . fromConstant

instance {-# OVERLAPPING #-} FromConstant (Vec f c) (Vec f c)

instance Arbitrary (f c) => Arbitrary (Vec f c) where
  arbitrary = Vec <$> arbitrary

instance
  (Symbolic c, Traversable f, Representable f)
  => MultiplicativeSemigroup (Vec f c)
  where
  Vec _v1 * Vec _v2 = Vec $ Haskell.error "TODO"
      -- fromCircuit2F v1 v2 $
      --   mzipWithMRep
      --    ( \i j ->
      --        newAssigned $ ($ i) * ($ j)
      --    )

instance Symbolic c => Scale Natural (Vec f c) where
  scale = Haskell.error "TODO"

instance Symbolic c => Scale Integer (Vec f c) where
  scale = Haskell.error "TODO"

instance Symbolic c => Exponent (Vec f c) Natural where
  Vec _v ^ _n = Vec $ Haskell.error "TODO" -- fromCircuitF v $ traverse (\i -> newAssigned $ ($ i) ^ n)

instance
  (Symbolic c, Traversable f, Representable f)
  => AdditiveSemigroup (Vec f c)
  where
  Vec _v1 + Vec _v2 = Vec $ Haskell.error "TODO"
      -- fromCircuit2F v1 v2 $
      --   mzipWithMRep
      --     ( \i j ->
      --         newAssigned $ ($ i) + ($ j)
      --     )

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
  negate (Vec _v) = Vec $ Haskell.error "TODO"
      -- fromCircuitF v $
      --   traverse
      --     ( \i ->
      --         newAssigned $ negate ($ i)
      --     )

instance (Symbolic c, Traversable f, Representable f) => Semiring (Vec f c)

instance (Symbolic c, Traversable f, Representable f) => Ring (Vec f c)
