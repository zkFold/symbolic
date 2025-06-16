{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.Conditional (module ZkFold.Control.Conditional) where

import           Data.Function                    (($))
import           GHC.Generics                     (Par1 (..))
import qualified Prelude

import           ZkFold.Algebra.Class
import           ZkFold.Control.Conditional
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool        (Bool (Bool))
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators (mzipWithMRep)
import           ZkFold.Symbolic.MonadCircuit     (newAssigned)

-- TODO: move to ZkFold.Symbolic.Data.Bool
-- TODO: Can we have a conditional on symbolic functions?

instance (SymbolicData x, Symbolic c) => Conditional (Bool c) (x c) where
  bool x y (Bool (Sym b)) = fromContext $
    ( symbolic3F b (toContext x) (toContext y)
        (\(Par1 c) f t -> if c Prelude.== zero then f else t)
        \(Par1 c) -> mzipWithMRep $ \i j -> do
          i' <- newAssigned (\w -> (one - w c) * w i)
          j' <- newAssigned (\w -> w c * w j)
          newAssigned (\w -> w i' + w j')
    )
