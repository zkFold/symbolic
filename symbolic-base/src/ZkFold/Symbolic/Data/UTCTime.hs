{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.UTCTime where

import GHC.Natural (Natural)
import Prelude hiding (Bool, Eq, Ord)

import ZkFold.Algebra.Class (FromConstant)
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq (Eq)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Ord (Ord)
import ZkFold.Symbolic.Data.UIntData (UIntData)
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)

newtype UTCTime c = UTCTime (UIntData 11 11 c)
  deriving newtype (Collect (ConstrainedDatum c), Eq, Ord, SymbolicData)

deriving newtype instance
  FromConstant Natural c => FromConstant Natural (UTCTime c)
