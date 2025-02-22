module ZkFold.Symbolic.Ledger.Types.OutputRef where

import           Prelude                           hiding (Bool, Eq, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Data.Combinators  (RegisterSize (Auto))
import           ZkFold.Symbolic.Data.UInt         (UInt)
import           ZkFold.Symbolic.Ledger.Types.Hash (Hash)

-- | Transaction hash.
type TransactionId = Hash

-- | Index of an output in the transaction's output list.
type OutputIndex = UInt 32 Auto

-- | Reference to a transaction output.
data OutputRef context = OutputRef
    { refId  :: TransactionId context
    -- ^ The transaction id of the transaction that produced the output.
    , refIdx :: OutputIndex context
    -- ^ The index of the output in the transaction's output list.
    }
