{-# LANGUAGE UndecidableInstances #-}
module ZkFold.Symbolic.Ledger.Types.Transaction.Batch (
   TransactionBatch (..),
) where

import           GHC.Generics                                       (Generic)
import           Prelude                                            hiding (Bool, Eq, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Class                              (Symbolic)
import           ZkFold.Symbolic.Data.Bool                          (Bool)
import           ZkFold.Symbolic.Data.Class                         (SymbolicData (..))
import           ZkFold.Symbolic.Data.Combinators                   (KnownRegisters, RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Conditional                   (Conditional)
import           ZkFold.Symbolic.Data.Eq                            (Eq)
import           ZkFold.Symbolic.Data.List                          (List)
import           ZkFold.Symbolic.Data.UTCTime                       (UTCTime)
import           ZkFold.Symbolic.Ledger.Types.DataAvailability      (DAIndex)
import           ZkFold.Symbolic.Ledger.Types.Hash                  (HashSimple)
import           ZkFold.Symbolic.Ledger.Types.Transaction.Core      (KnownRegistersOutputIndex)
import           ZkFold.Symbolic.Ledger.Types.Value                 (KnownRegistersAssetQuantity)

-- TODO: Use POSIXTime instead of UTCTime?
-- | Defines the on-chain representation of the Symbolic Ledger state transition.
data TransactionBatch context = TransactionBatch
    { tbDataHashes       :: List context ((DAIndex context), HashSimple context)
    -- ^ Hash of 'TransactionBatchData' indexed by the corresponding data availability source.
    , tbBridgeIn         :: HashSimple context
    -- ^ Hash of the 'AssetValues' that are bridged into the ledger.
    , tbBridgeOut        :: HashSimple context
    -- ^ Hash of the 'AssetValues' that are bridged out of the ledger.
    , tbValidityInterval :: (UTCTime context, UTCTime context)
    -- ^ The validity interval of the transaction batch. The bounds are inclusive.
    , tbPreviousBatch    :: HashSimple context
    -- ^ Hash of the previous transaction batch.
    }
    deriving stock Generic

instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, KnownRegisters context 11 Auto, Symbolic context) => SymbolicData (TransactionBatch context)
instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, KnownRegisters context 11 Auto, Symbolic context) => Conditional (Bool context) (TransactionBatch context)
instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, KnownRegisters context 11 Auto, Symbolic context) => Eq (TransactionBatch context)
