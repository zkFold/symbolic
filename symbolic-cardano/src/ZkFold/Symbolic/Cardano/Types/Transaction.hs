{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Cardano.Types.Transaction where

import GHC.Generics (Generic)
import ZkFold.Algebra.Number
import ZkFold.Data.HFunctor.Classes (HEq)
import ZkFold.Data.Vector
import ZkFold.Symbolic.Cardano.Types.Basic
import ZkFold.Symbolic.Cardano.Types.Input (Input)
import ZkFold.Symbolic.Cardano.Types.Output (Liability (..), Output)
import ZkFold.Symbolic.Cardano.Types.Value (Value)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.Input (SymbolicInput, isValid)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))
import qualified Prelude as Haskell

data Transaction inputs rinputs outputs tokens mint datum context = Transaction
  { txRefInputs :: Vector rinputs (Input tokens datum context)
  , txInputs :: Vector inputs (Input tokens datum context)
  , txOutputs :: Vector outputs (Output tokens datum context)
  , txLiability :: Liability context
  , txMint :: Value mint context
  , txTime :: (UTCTime context, UTCTime context)
  }

deriving instance Generic (Transaction inputs rinputs outputs tokens mint datum context)

deriving instance
  HEq context
  => Haskell.Eq (Transaction inputs rinputs outputs tokens mint datum context)

-- TODO: Think how to prettify this abomination
deriving instance
  ( Symbolic context
  , KnownRegisters context 32 Auto
  , KnownRegisters context 64 Auto
  , KnownRegisters context 11 Auto
  , KnownNat tokens
  , KnownNat rinputs
  , KnownNat inputs
  , KnownNat outputs
  , KnownNat mint
  )
  => SymbolicData (Transaction inputs rinputs outputs tokens mint datum context)

instance
  ( Symbolic context
  , KnownRegisters context 32 Auto
  , KnownRegisters context 64 Auto
  , KnownRegisters context 11 Auto
  , KnownNat tokens
  , KnownNat rinputs
  , KnownNat outputs
  , KnownNat mint
  , KnownNat inputs
  )
  => SymbolicInput (Transaction inputs rinputs outputs tokens mint datum context)
  where
  isValid (Transaction _ txI _ _ _ _) = isValid txI
