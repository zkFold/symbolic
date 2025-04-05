{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE UndecidableInstances #-}
module ZkFold.Symbolic.Ledger.Types.Transaction.Core (
    Transaction (txInputs, txOutputs, txValidityInterval, txOwner),
    TransactionId,
    txId,
    OutputIndex,
    KnownRegistersOutputIndex,
    OutputRef (..),
    Input (..),
) where

import           GHC.Generics                          (Generic)
import           Prelude                               hiding (Bool, Eq, Maybe, length, splitAt, (*), (+), (==), (||))
import qualified Prelude                               as Haskell hiding ((||))

import           ZkFold.Symbolic.Class                 (Symbolic)
import           ZkFold.Symbolic.Data.Bool             (Bool, BoolType (..))
import           ZkFold.Symbolic.Data.Class            (SymbolicData (..))
import           ZkFold.Symbolic.Data.Combinators      (KnownRegisters, RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Conditional      (Conditional, ifThenElse)
import           ZkFold.Symbolic.Data.Eq               (Eq (..))
import           ZkFold.Symbolic.Data.Hash             (Hashable, hash)
import qualified ZkFold.Symbolic.Data.List             as Symbolic.List
import           ZkFold.Symbolic.Data.List             (List)
import           ZkFold.Symbolic.Data.Maybe            (Maybe, just, nothing)
import           ZkFold.Symbolic.Data.Morph            (MorphTo (..))
import           ZkFold.Symbolic.Data.UInt             (UInt)
import           ZkFold.Symbolic.Data.UTCTime          (UTCTime)
import           ZkFold.Symbolic.Fold                  (SymbolicFold)
import           ZkFold.Symbolic.Ledger.Types.Address  (Address)
import           ZkFold.Symbolic.Ledger.Types.Hash     (Hash, HashSimple)
import           ZkFold.Symbolic.Ledger.Types.Interval (Interval)
import           ZkFold.Symbolic.Ledger.Types.Output   (Output (..))
import           ZkFold.Symbolic.Ledger.Types.Value    (KnownRegistersAssetQuantity)

-- TODO: Use POSIXTime instead of UTCTime?
-- | Transaction in our symbolic ledger.
data Transaction context = Transaction
    { txInputs           :: List context (Input context)
    -- ^ A list of inputs to the transaction.
    , txOutputs          :: List context (Output context)
    -- ^ A list of outputs of the transaction.
    , txValidityInterval :: Interval context
    -- ^ The validity interval of the transaction. The bounds are inclusive.
    , txOwner            :: Address context
    -- ^ Inputs belonging to this address are considered spent whereas others are considered to be only referenced by this transaction.
    }
    deriving stock Generic

instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, KnownRegisters context 11 Auto, Symbolic context) => SymbolicData (Transaction context)
instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, KnownRegisters context 11 Auto, Symbolic context) => Conditional (Bool context) (Transaction context)
instance (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, KnownRegisters context 11 Auto, Symbolic context) => Eq (Transaction context)

-- | Builds a 'Transaction' validating that there is at least one input belonging to the "owner".
mkTransaction :: forall context. (SymbolicFold context, KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, KnownRegisters context 11 Auto) => List context (Input context) -> List context (Output context) -> (UTCTime context, UTCTime context) -> Address context -> Maybe context (Transaction context)
mkTransaction inputs outputs validityInterval owner =
    let (hasOwnerInput, _) = Symbolic.List.foldl (Morph \((accBool :: Bool s, owner'), x :: Input s) -> (accBool || (txoAddress (txiOutput x) == owner'), owner')) (false :: Bool context, owner) inputs
    in ifThenElse hasOwnerInput nothing (just $ Transaction {txInputs = inputs, txOutputs = outputs, txValidityInterval = validityInterval, txOwner = owner})

-- | Transaction hash.
type TransactionId context = Hash (Transaction context)

txId :: (KnownRegistersAssetQuantity context, KnownRegistersOutputIndex context, KnownRegisters context 11 Auto, Symbolic context, Hashable (HashSimple context) (Transaction context)) => Transaction context -> TransactionId context
txId = hash

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
