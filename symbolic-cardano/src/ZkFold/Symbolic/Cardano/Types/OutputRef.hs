{-# LANGUAGE UndecidableInstances #-}
-- Avoid reduction overflow error caused by NumberOfRegisters
{-# OPTIONS_GHC -freduction-depth=0 #-}

module ZkFold.Symbolic.Cardano.Types.OutputRef where

import GHC.Generics (Generic)
import ZkFold.Data.HFunctor.Classes (HEq)
import ZkFold.Symbolic.Cardano.Types.Basic
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators (KnownRegisters, RegisterSize (..))
import ZkFold.Symbolic.Data.Conditional
import ZkFold.Symbolic.Data.Eq
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))
import qualified Prelude as Haskell

type TxRefId context = ByteString 256 context

type TxRefIndex context = UInt 32 Auto context

data OutputRef context = OutputRef
  { outputRefId :: TxRefId context
  , outputRefIndex :: TxRefIndex context
  }
  deriving Generic

deriving instance HEq context => Haskell.Eq (OutputRef context)

instance
  (KnownRegisters context 32 Auto, Symbolic context)
  => SymbolicData (OutputRef context)

instance
  (KnownRegisters context 32 Auto, Symbolic context)
  => SymbolicInput (OutputRef context)

instance
  (KnownRegisters context 32 Auto, Symbolic context)
  => Conditional (Bool context) (OutputRef context)

instance
  (KnownRegisters context 32 Auto, Symbolic context)
  => Eq (OutputRef context)
