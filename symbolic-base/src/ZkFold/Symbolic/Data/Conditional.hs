{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.Conditional (module ZkFold.Control.Conditional) where

import Data.Function (($))
import Data.Functor.Rep (mzipWithRep)
import Data.Proxy
import GHC.Generics (Par1 (..))
import qualified Prelude

import ZkFold.Algebra.Class
import ZkFold.Control.Conditional
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Bool (Bool (Bool))
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators (mzipWithMRep)
import ZkFold.Symbolic.MonadCircuit (newAssigned)

-- TODO: move to ZkFold.Symbolic.Data.Bool

instance (LayoutFunctor f, Symbolic c) => Conditional (Bool c) (c f) where
  bool x y (Bool b) = restore $ \s ->
    ( symbolic3F
        b
        (arithmetize x s)
        (arithmetize y s)
        (\(Par1 c) f t -> if c Prelude.== zero then f else t)
        \(Par1 c) -> mzipWithMRep $ \i j -> do
          i' <- newAssigned (\w -> (one - w c) * w i)
          j' <- newAssigned (\w -> w c * w j)
          newAssigned (\w -> w i' + w j')
    , let Par1 wb = witnessF b
       in mzipWithRep
            (\wx wy -> (one - wb) * wx + wb * wy)
            (payload x s)
            (payload y s)
    )

deriving newtype instance Symbolic c => Conditional (Bool c) (Bool c)

instance Symbolic c => Conditional (Bool c) (Proxy c) where
  bool _ _ _ = Proxy
