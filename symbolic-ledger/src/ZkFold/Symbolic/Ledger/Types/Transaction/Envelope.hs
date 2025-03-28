{-# LANGUAGE UndecidableInstances #-}
module ZkFold.Symbolic.Ledger.Types.Transaction.Envelope (
    TransactionEnvelope (..),
) where

import           GHC.Generics                                  (Generic)
import           Prelude                                       hiding (Bool, Eq, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Class                         (Symbolic)
import           ZkFold.Symbolic.Data.Bool                     (Bool)
import           ZkFold.Symbolic.Data.Class                    (SymbolicData (..))
import           ZkFold.Symbolic.Data.Combinators              (KnownRegisters, RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Conditional              (Conditional)
import           ZkFold.Symbolic.Data.Eq                       (Eq)
import           ZkFold.Symbolic.Ledger.Types.Address          (Address)
import           ZkFold.Symbolic.Ledger.Types.Hash             (HashSimple)
import           ZkFold.Symbolic.Ledger.Types.Transaction.Core (KnownRegistersOutputIndex, TransactionId)
import           ZkFold.Symbolic.Ledger.Types.Value            (KnownRegistersAssetQuantity)

-- | Envelope of a transaction.
data TransactionEnvelope context = TransactionEnvelope
    { txeAddress       :: Address context
    -- ^ Address that initiated the transaction. Inputs in the transaction that are locked by this address are considered to be spent whereas rest are considered as reference inputs.
    , txeTransactionId :: HashSimple context
    -- ^ 'TransactionId'.
    }
    deriving stock Generic

instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, KnownRegisters context 11 Auto, Symbolic context) => SymbolicData (TransactionEnvelope context)
instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, KnownRegisters context 11 Auto, Symbolic context) => Conditional (Bool context) (TransactionEnvelope context)
instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, KnownRegisters context 11 Auto, Symbolic context) => Eq (TransactionEnvelope context)
