{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Cardano.Types.Transaction where

import GHC.Generics (Generic, Generic1, (:*:))
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Vector
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

import ZkFold.Symbolic.Cardano.Types.Basic
import ZkFold.Symbolic.Cardano.Types.Input (Input)
import ZkFold.Symbolic.Cardano.Types.Output (Liability (..), Output)
import ZkFold.Symbolic.Cardano.Types.Value (Value)

data Transaction inputs rinputs outputs tokens mint datum context = Transaction
  { txRefInputs :: Vector rinputs (Input tokens datum context)
  , txInputs :: Vector inputs (Input tokens datum context)
  , txOutputs :: Vector outputs (Output tokens datum context)
  , txLiability :: Liability context
  , txMint :: Value mint context
  , txTime :: (UTCTime :*: UTCTime) context
  }
  deriving (Generic, Generic1, SymbolicData)

instance Symbolic c => Collect (ConstrainedDatum c) (Transaction i r o t m d c)
