{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Validation.TransactionBatch (
  TransactionBatchWitness (..),
  validateTransactionBatch,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic, Generic1, (:*:) (..), (:.:) (..))
import GHC.TypeNats (KnownNat, type (-))
import ZkFold.Algebra.Class (Zero (..), one, (+))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq ((==))
import ZkFold.Data.HFunctor.Classes (HShow)
import ZkFold.Data.Vector (Vector, Zip (..))
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Utils (unsafeToVector')
import ZkFold.Symbolic.Ledger.Validation.Transaction (TransactionWitness, validateTransaction)

-- | Transaction batch witness for validating transaction batch.
newtype TransactionBatchWitness ud s n a t context = TransactionBatchWitness
  { tbwTransactions :: (Vector t :.: TransactionWitness ud s n a) context
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

deriving stock instance HShow context => Haskell.Show (TransactionBatchWitness ud s n a t context)

deriving anyclass instance ToJSON (TransactionBatchWitness ud s n a t RollupBFInterpreter)

deriving anyclass instance
  forall ud s n a t. (KnownNat s, KnownNat n) => FromJSON (TransactionBatchWitness ud s n a t RollupBFInterpreter)

deriving anyclass instance
  forall ud s n a t
   . (KnownNat ud, KnownNat s, KnownNat n, KnownNat a, KnownNat t, KnownNat (ud - 1))
  => ToSchema (TransactionBatchWitness ud s n a t RollupBFInterpreter)

-- | Validate transaction batch. See note [State validation] for details.
validateTransactionBatch
  :: forall ud s bo n a t context
   . SignatureTransactionBatch ud s n a t context
  => FieldElement context
  -- ^ UTxO tree root hash.
  -> (Vector bo :.: Output a) context
  -- ^ Bridged out outputs.
  -> TransactionBatch n a t context
  -- ^ Transaction batch.
  -> TransactionBatchWitness ud s n a t context
  -- ^ Witness for the transaction batch.
  -> ( Bool
         :*: FieldElement
         :*: (Vector t :.: (Vector n :.: FieldElement))
         :*: (Vector t :.: (Vector n :.: (Bool :*: FieldElement :*: FieldElement)))
     )
       context
  -- ^ Result of validation:
  -- (1) validity,
  -- (2) updated UTxO tree root hash,
  -- (3) input deltas per transaction: packed leaf positions,
  -- (4) output deltas per transaction: (isActive, packed position, new hash).
validateTransactionBatch utxoRoot bridgedOutOutputs tb tbw =
  let
    transactionBatchWithWitness = zipWith (:*:) tb.tbTransactions (unComp1 tbw.tbwTransactions)
    ((boCount :*: isValid :*: updatedRoot), inputDeltasRev, outputDeltasRev) =
      foldl'
        ( \((boCountAcc :*: isValidAcc :*: rootAcc), inDsAcc, outDsAcc) (tx :*: txw) ->
            let (txBOuts :*: isTxValid :*: newRootAcc :*: txInputDelta :*: txOutputDelta) =
                  validateTransaction rootAcc bridgedOutOutputs tx txw
             in ( (boCountAcc + txBOuts) :*: (isValidAcc && isTxValid) :*: newRootAcc
                , txInputDelta : inDsAcc
                , txOutputDelta : outDsAcc
                )
        )
        ((zero :: FieldElement context) :*: true :*: utxoRoot, [], [])
        transactionBatchWithWitness
    batchInputDeltas = Comp1 (unsafeToVector' (Haskell.reverse inputDeltasRev))
    batchOutputDeltas = Comp1 (unsafeToVector' (Haskell.reverse outputDeltasRev))
    bouts =
      foldl'
        (\acc output -> ifThenElse (output == nullOutput @a @context) acc (acc + one))
        zero
        (unComp1 bridgedOutOutputs)
   in
    ( (isValid && (bouts == boCount))
        :*: updatedRoot
        :*: batchInputDeltas
        :*: batchOutputDeltas
    )
