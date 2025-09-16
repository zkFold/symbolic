{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Cardano.Types.Transaction where

import GHC.Generics (Generic, Generic1, (:*:), type (:.:) (..))
import ZkFold.Data.HFunctor.Classes (HEq)
import ZkFold.Data.Vector
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))
import qualified Prelude as Haskell

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

deriving instance
  HEq context
  => Haskell.Eq (Transaction inputs rinputs outputs tokens mint datum context)

instance SymbolicInput (Transaction inputs rinputs outputs tokens mint datum) where
  isValid (Transaction _ txI _ _ _ _) = isValid (Comp1 txI)
