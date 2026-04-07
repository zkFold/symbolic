{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Validation.State (
  validateStateUpdate,
  validateStateUpdateIndividualChecks,
  StateWitness (..),
) where

import Control.Lens ((&))
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic, Generic1, (:*:) (..), (:.:) (..))
import GHC.TypeNats (KnownNat, type (-))
import ZkFold.Algebra.Class (MultiplicativeMonoid (..), Zero (..), (+))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq (Eq (..), (==))
import ZkFold.Data.HFunctor.Classes (HShow)
import ZkFold.Data.Vector (Vector, Zip (..))
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..), true)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hashable (..), hash)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry, packIndex)
import ZkFold.Symbolic.Data.MerkleTree qualified as MerkleTree
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Types.Field
import ZkFold.Symbolic.Ledger.Utils (unsafeToVector')
import ZkFold.Symbolic.Ledger.Validation.Transaction (outputHasValueSanity)
import ZkFold.Symbolic.Ledger.Validation.TransactionBatch (TransactionBatchWitness, validateTransactionBatch)

{- Note [State validation]

For validating transactions, we should check:

\* Inputs come from UTxO set.
\* We subtract inputs UTxOs from the UTxO set.
\* Outputs are added to the UTxO set if they are not bridge out outputs.
\* We require signature from addresses corresponding to spent UTxOs.
\* Bridged out outputs are checked to be same as in bridge out list.
\* Outputs (bridged out or not) must have at least one ada and all assets are non-negative.
\* Transaction is balanced.
\* Transaction must have at least one input OR the entire transaction is null (no inputs & no outputs)
\* Bridged out output is not a null output.

For validating batch, we simply apply transaction validation check iteratively.

For validating state, we check following:

\* Previous state hash is correctly set.
\* New UTxO state is properly computed. For it, we first added UTxOs to the old state corresponding to bridged in assets and then update this set by folding over transactions.
\* Bridge in outputs have at least one ada and all assets are non-negative.
\* Transaction batch is valid.
\* Length is incremented by one.
\* Bridged out list is correctly computed.
-}

-- | State witness for validating state update.
data StateWitness bi bo ud a s n t context = StateWitness
  { swBridgeIn :: (Vector bi :.: Output a) context
  -- ^ Outputs that are bridged into the ledger. These lead to creation of new UTxOs where `orTxId` of the output is obtained by hashing `sLength` and `orIndex` is the index of the output in the vector.
  , swBridgeOut :: (Vector bo :.: Output a) context
  -- ^ Denotes outputs that are bridged out of the ledger.
  , swAddBridgeIn :: (Vector bi :.: MerkleEntry ud) context
  , swTransactionBatch :: (TransactionBatchWitness ud s n a t) context
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

deriving stock instance HShow context => Haskell.Show (StateWitness bi bo ud a s n t context)

deriving anyclass instance ToJSON (StateWitness bi bo ud a s n t RollupBFInterpreter)

deriving anyclass instance
  forall bi bo ud a s n t. (KnownNat s, KnownNat n) => FromJSON (StateWitness bi bo ud a s n t RollupBFInterpreter)

deriving anyclass instance
  forall bi bo ud a s n t
   . (KnownNat bi, KnownNat bo, KnownNat (ud - 1), KnownNat ud, KnownNat a, KnownNat s, KnownNat n, KnownNat t)
  => ToSchema (StateWitness bi bo ud a s n t RollupBFInterpreter)

-- | Validate state update. See note [State validation] for details.
-- Returns validity and tree delta:
-- (1) validity boolean,
-- (2) bridge-in deltas: (isActive, packed position, new leaf hash) per bridge-in,
-- (3) input deltas per transaction: packed positions,
-- (4) output deltas per transaction: (isActive, packed position, new hash).
validateStateUpdate
  :: forall bi bo ud a s n t context
   . SignatureState bi bo context
  => SignatureTransactionBatch ud s n a t context
  => State context
  -- ^ Previous state.
  -> TransactionBatch n a t context
  -- ^ The "action" that is applied to the state.
  -> State context
  -- ^ New state.
  -> StateWitness bi bo ud a s n t context
  -- ^ Witness for the state.
  -> ( Bool
         :*: (Vector bi :.: (Bool :*: FieldElement :*: FieldElement))
         :*: (Vector t :.: (Vector n :.: FieldElement))
         :*: (Vector t :.: (Vector n :.: (Bool :*: FieldElement :*: FieldElement)))
     )
       context
validateStateUpdate previousState action newState sw =
  let (checks, biDelta, inDeltas, outDeltas) =
        validateStateUpdateIndividualChecks previousState action newState sw
      isValid = checks == Haskell.pure true
   in isValid :*: biDelta :*: inDeltas :*: outDeltas

-- | Validate state update and return either the first failing reason or success,
-- along with tree delta data.
validateStateUpdateIndividualChecks
  :: forall bi bo ud a s n t context
   . SignatureState bi bo context
  => SignatureTransactionBatch ud s n a t context
  => State context
  -- ^ Previous state.
  -> TransactionBatch n a t context
  -- ^ The "action" that is applied to the state.
  -> State context
  -- ^ New state.
  -> StateWitness bi bo ud a s n t context
  -- ^ Witness for the state.
  -> ( Vector 5 (Bool context)
     , (Vector bi :.: (Bool :*: FieldElement :*: FieldElement)) context
     , (Vector t :.: (Vector n :.: FieldElement)) context
     , (Vector t :.: (Vector n :.: (Bool :*: FieldElement :*: FieldElement))) context
     )
validateStateUpdateIndividualChecks previousState action newState sw =
  let
    initialRoot = previousState.sUTxO
    bridgeInAssets = sw.swBridgeIn
    bridgedInAssetsWithWitness = zipWith (:*:) (unComp1 bridgeInAssets) (unComp1 sw.swAddBridgeIn)

    bridgeInHash = newState.sLength & hash & Base.hHash
    (_ :*: isWitBridgeInValid :*: rootWithBridgeIn, biDeltasRev) =
      foldl'
        ( \(ix :*: isValidAcc :*: rootAcc, deltasAcc) ((output :*: merkleEntry)) ->
            let nullUTxOHash' = nullUTxOHash @a @context
                isNull = output == nullOutput
                utxo = UTxO {uRef = OutputRef {orTxId = bridgeInHash, orIndex = ix}, uOutput = output}
                utxoHash = hash utxo & Base.hHash
                pos = packIndex (MerkleTree.position merkleEntry)
                isActive = not isNull
                deltaEntry = isActive :*: pos :*: utxoHash
                (isInTree, updatedRoot) = MerkleTree.containsAndReplaceRoot merkleEntry utxoHash rootAcc
                isValid' =
                  isValidAcc
                    && ifThenElse
                      isNull
                      true
                      ( isInTree
                          && (MerkleTree.value merkleEntry == nullUTxOHash')
                          && outputHasValueSanity output
                      )
             in ( (ix + one)
                    :*: isValid'
                    :*: ifThenElse
                      (isValid' && not isNull)
                      updatedRoot
                      rootAcc
                , deltaEntry : deltasAcc
                )
        )
        (zero :*: true :*: initialRoot, [])
        bridgedInAssetsWithWitness
    bridgeInDelta = Comp1 (unsafeToVector' (Haskell.reverse biDeltasRev))
    bridgedOutOutputs = sw.swBridgeOut
    (isBatchValid :*: finalRoot :*: batchInputDeltas :*: batchOutputDeltas) =
      validateTransactionBatch rootWithBridgeIn bridgedOutOutputs action sw.swTransactionBatch
   in
    ( unsafeToVector'
        [ newState.sPreviousStateHash == hasher previousState
        , newState.sLength == previousState.sLength + one
        , isWitBridgeInValid
        , isBatchValid
        , finalRoot == newState.sUTxO
        ]
    , bridgeInDelta
    , batchInputDeltas
    , batchOutputDeltas
    )
