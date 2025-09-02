{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.UPLC.Fun (Fun (..)) where

import Data.Function ((.))
import ZkFold.Algebra.Class (FromConstant (..))
import ZkFold.Symbolic.Data.Maybe qualified as Symbolic

import ZkFold.Symbolic.UPLC.Class
import ZkFold.UPLC.BuiltinType
import ZkFold.Symbolic.Data.Class (HasRep)

-- | Symbolic function of a definite UPLC signature.
data Fun (s :: [BuiltinType]) (t :: BuiltinType) c where
  -- | Fully applied (saturated) function.
  FSat :: (IsData t v, HasRep v c) => Symbolic.Maybe v c -> Fun '[] t c
  -- | A function which returns another (possibly saturated) function.
  FLam :: IsData s v => (v c -> Fun ss t c) -> Fun (s ': ss) t c

instance (IsData t v, HasRep v c) => FromConstant (Symbolic.Maybe v c) (Fun '[] t c) where
  fromConstant = FSat

instance
  (IsData s v, FromConstant f (Fun ss t c))
  => FromConstant (v c -> f) (Fun (s ': ss) t c)
  where
  fromConstant f = FLam (fromConstant . f)
