{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module ZkFold.Symbolic.Ledger.Types.Transaction where

import           GHC.Generics                          (Generic)
import           Prelude                               hiding (Bool, Eq, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Class                 (Symbolic)
import           ZkFold.Symbolic.Data.Bool             (Bool)
import           ZkFold.Symbolic.Data.Class            (SymbolicData (..))
import           ZkFold.Symbolic.Data.Combinators      (KnownRegisters, RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Conditional      (Conditional)
import           ZkFold.Symbolic.Data.Eq               (Eq)
import           ZkFold.Symbolic.Data.List             (List)
import           ZkFold.Symbolic.Data.UInt             (UInt)
import           ZkFold.Symbolic.Data.UTCTime          (UTCTime)
import           ZkFold.Symbolic.Ledger.Types.Contract
import           ZkFold.Symbolic.Ledger.Types.Hash     (Hash, HashSimple)
import           ZkFold.Symbolic.Ledger.Types.Output   (Output)
import           ZkFold.Symbolic.Ledger.Types.Value    (AssetValue, KnownRegistersAssetQuantity)

-- | zkFold ledger transaction.
data Transaction context = Transaction
    { txInputs  :: List context (Input context)
    -- ^ A list of inputs to the transaction.
    , txOutputs :: List context (Output context)
    -- ^ A list of outputs of the transaction.
    }
    deriving stock Generic

instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, Symbolic context) => SymbolicData (Transaction context)
instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, Symbolic context) => Conditional (Bool context) (Transaction context)
instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, Symbolic context) => Eq (Transaction context)

-- | Transaction hash.
type TransactionId context = Hash (Transaction context)

txId :: Transaction context -> TransactionId context
txId = undefined

-- | Index of an output in the transaction's output list.
type OutputIndex = UInt 32 Auto

type KnownRegistersOutputIndex context = KnownRegisters context 32 Auto

-- | Reference to a transaction output.
data OutputRef context = OutputRef
    { refId  :: HashSimple context
    -- ^ The transaction id of the transaction that produced the output.
    , refIdx :: OutputIndex context
    -- ^ The index of the output in the transaction's output list.
    }
    deriving stock Generic

instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, Symbolic context) => SymbolicData (OutputRef context)
instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, Symbolic context) => Conditional (Bool context) (OutputRef context)
instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, Symbolic context) => Eq (OutputRef context)

-- | Input to a transaction.
data Input context = Input
    { txiOutputRef :: OutputRef context
    -- ^ Reference to the output being spent.
    , txiOutput    :: Output context
    -- ^ The output being spent.
    }
    deriving stock Generic

instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, Symbolic context) => SymbolicData (Input context)
instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, Symbolic context) => Conditional (Bool context) (Input context)
instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, Symbolic context) => Eq (Input context)
