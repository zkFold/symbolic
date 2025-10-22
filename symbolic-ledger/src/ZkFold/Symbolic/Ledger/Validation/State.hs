{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Symbolic.Ledger.Validation.State (
  validateStateUpdate,
  validateStateUpdateEither,
  StateWitness (..),
  StateUpdateValidationError (..),
  Either' (..),
  StateUpdateValidationResult,
) where

import Data.Function ((&))
import GHC.Generics ((:*:) (..), (:.:) (..))
import GHC.Generics qualified as G
import ZkFold.Algebra.Class (MultiplicativeMonoid (..), Zero (..), (+))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq (Eq (..), (==))
import ZkFold.Data.Vector (Vector, Zip (..))
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..), true)
import ZkFold.Symbolic.Data.Hash (Hashable (..), hash, preimage)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry)
import ZkFold.Symbolic.Data.MerkleTree qualified as MerkleTree
import ZkFold.Symbolic.Data.Sum (Sum, inject, match)

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.Transaction (outputHasAtLeastOneAda)
import ZkFold.Symbolic.Ledger.Validation.TransactionBatch (TransactionBatchWitness, validateTransactionBatch)
import ZkFold.Data.HFunctor.Classes (HShow)
import qualified Prelude as Haskell

{- Note [State validation]

For validating transactions, we should check:

\* Inputs come from UTxO set.
\* We subtract inputs UTxOs from the UTxO set.
\* Outputs are added to the UTxO set if they are not bridge out outputs.
\* We require signature from addresses corresponding to spent UTxOs.
\* Bridged out outputs are checked to be same as in bridge out list.
\* Outputs must have at least one ada.
\* Transaction is balanced.
\* Transaction must have at least one input.

For validating batch, we simply apply transaction validation check iteratively.

For validating state, we check following:

\* Previous state hash is correctly set.
\* New UTxO state is properly computed. For it, we first added UTxOs to the old state corresponding to bridged in assets and then update this set by folding over transactions.
\* Bridge in outputs have at least one ada.
\* Transaction batch is valid.
\* Length is incremented by one.
\* Bridged out list is correctly computed.
-}

-- | State witness for validating state update.
data StateWitness bi bo ud a i o t context = StateWitness
  { swAddBridgeIn :: (Vector bi :.: MerkleEntry ud) context
  , swTransactionBatch :: (TransactionBatchWitness ud i o a t) context
  }

-- | Error cases for state update validation.
data StateUpdateValidationError c
  = PreviousStateHashMismatch
  | LengthNotIncrementedByOne
  | InvalidBridgeInWitness
  | InvalidTransactionBatch
  | UTxOTreeMismatch
  deriving (G.Generic, G.Generic1)

deriving stock instance (HShow context) => Haskell.Show (StateUpdateValidationError context)

-- | A minimal Either-like sum for symbolic types.
data Either' l r c
  = Left' (Sum l c)
  | Right' (r c)
  deriving (G.Generic, G.Generic1)

deriving stock instance (HShow context, Haskell.Show (r context), Haskell.Show (Sum l context)) => Haskell.Show (Either' l r context)

-- | Concrete result type for state update validation with error information.
type StateUpdateValidationResult = Sum (Either' StateUpdateValidationError Bool)

-- | Validate state update. See note [State validation] for details.
validateStateUpdate
  :: forall bi bo ud a i o t context
   . SignatureState bi bo ud a context
  => SignatureTransactionBatch ud i o a t context
  => State bi bo ud a context
  -- ^ Previous state.
  -> TransactionBatch i o a t context
  -- ^ The "action" that is applied to the state.
  -> State bi bo ud a context
  -- ^ New state.
  -> StateWitness bi bo ud a i o t context
  -- ^ Witness for the state.
  -> Bool context
validateStateUpdate previousState action newState sw =
  let res = validateStateUpdateEither previousState action newState sw
   in match res (\e -> case e of
        Left' _ -> false
        Right' b -> b)

-- | Validate state update and return either the first failing reason or success.
validateStateUpdateEither
  :: forall bi bo ud a i o t context
   . SignatureState bi bo ud a context
  => SignatureTransactionBatch ud i o a t context
  => State bi bo ud a context
  -- ^ Previous state.
  -> TransactionBatch i o a t context
  -- ^ The "action" that is applied to the state.
  -> State bi bo ud a context
  -- ^ New state.
  -> StateWitness bi bo ud a i o t context
  -- ^ Witness for the state.
  -> StateUpdateValidationResult context
validateStateUpdateEither previousState action newState sw =
  let
    initialUTxOTree = previousState.sUTxO
    bridgeInAssets = preimage newState.sBridgeIn
    bridgedInAssetsWithWitness = zipWith (:*:) (unComp1 bridgeInAssets) (unComp1 sw.swAddBridgeIn)

    bridgeInHash = newState.sLength & hash & Base.hHash
    (_ :*: isWitBridgeInValid :*: utxoTreeWithBridgeIn) =
      foldl'
        ( \(ix :*: isValidAcc :*: acc) ((output :*: merkleEntry)) ->
            let nullUTxOHash' = nullUTxOHash @a @context
                isValid' =
                  isValidAcc
                    && ifThenElse
                      (output == nullOutput)
                      true
                      ( (acc `MerkleTree.contains` merkleEntry)
                          && (merkleEntry.value == nullUTxOHash')
                          && outputHasAtLeastOneAda output
                      )
                utxo = UTxO {uRef = OutputRef {orTxId = bridgeInHash, orIndex = ix}, uOutput = output}
                utxoHash = hash utxo & Base.hHash
             in ( (ix + one)
                    :*: isValid'
                    :*: ifThenElse
                      (isValid' && (output /= nullOutput))
                      ( MerkleTree.replace
                          ( merkleEntry
                              { MerkleTree.value = utxoHash
                              }
                          )
                          acc
                      )
                      acc
                )
        )
        (zero :*: true :*: initialUTxOTree)
        bridgedInAssetsWithWitness
    bridgedOutOutputs = preimage newState.sBridgeOut
    (isBatchValid :*: utxoTree) = validateTransactionBatch utxoTreeWithBridgeIn bridgedOutOutputs action sw.swTransactionBatch
    condHash = newState.sPreviousStateHash == hasher previousState
    condLen  = newState.sLength == previousState.sLength + one
    condBridgeIn = isWitBridgeInValid
    condBatch = isBatchValid
    condUTxO = utxoTree == newState.sUTxO
   in
    -- Return the first failing reason, or success flag.
    ifThenElse condHash
      ( ifThenElse condLen
          ( ifThenElse condBridgeIn
              ( ifThenElse condBatch
                  ( ifThenElse condUTxO (inject (Right' true)) (inject (Left' (inject UTxOTreeMismatch))) )
                  (inject (Left' (inject InvalidTransactionBatch)))
              )
              (inject (Left' (inject InvalidBridgeInWitness)))
          )
          (inject (Left' (inject LengthNotIncrementedByOne)))
      )
      (inject (Left' (inject PreviousStateHashMismatch)))
