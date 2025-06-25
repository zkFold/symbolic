{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.UPLC.Data where

import GHC.Generics (Par1)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput)

-- | Plutus Core's Data as a Symbolic datatype.
-- TODO: Proper symbolic Data type
newtype Data c = Data (c Par1)

deriving newtype instance Symbolic c => SymbolicData (Data c)

deriving newtype instance Symbolic c => SymbolicInput (Data c)
