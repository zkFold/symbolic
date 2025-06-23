{-# LANGUAGE DataKinds #-}

module ZkFold.Symbolic.UPLC.Fun (Fun (..)) where

import Data.Function ((.))
import ZkFold.Algebra.Class (FromConstant (..))
import ZkFold.Symbolic.Data.Maybe qualified as Symbolic
import ZkFold.Symbolic.UPLC.Class
import ZkFold.UPLC.BuiltinType

-- | Symbolic function of a definite UPLC signature.
data Fun (s :: [BuiltinType]) (t :: BuiltinType) c where
  -- | Fully applied (saturated) function.
  FSat :: IsData t v c => Symbolic.Maybe c v -> Fun '[] t c
  -- | A function which returns another (possibly saturated) function.
  FLam :: IsData s v c => (v -> Fun ss t c) -> Fun (s ': ss) t c

instance IsData t v c => FromConstant (Symbolic.Maybe c v) (Fun '[] t c) where
  fromConstant = FSat

instance
  (FromConstant f (Fun ss t c), IsData s v c)
  => FromConstant (v -> f) (Fun (s ': ss) t c)
  where
  fromConstant f = FLam (fromConstant . f)
