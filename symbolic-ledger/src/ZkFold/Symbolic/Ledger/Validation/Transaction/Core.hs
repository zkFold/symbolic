{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction.Core (
  UTxO,
  validateTransaction,
  validateTransactionWithAssetDiff,
) where

import           Prelude                          (fst, undefined, ($))

import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Conditional (ifThenElse)
import           ZkFold.Symbolic.Data.Eq          ((==))
import qualified ZkFold.Symbolic.Data.List        as Symbolic.List
import           ZkFold.Symbolic.Data.List        (List)
import           ZkFold.Symbolic.Data.Morph
import           ZkFold.Symbolic.Ledger.Types

-- | UTxO set.
type UTxO context = List context (Input context)

-- | This function extracts boolean from 'validateTransaction', see it for more details.
validateTransaction ::
  forall context.
  Signature context =>
  -- | 'Transaction' to validate.
  Transaction context ->
  -- | Witness for 'Transaction' validation.
  UTxO context ->
  List context (Address context) ->
  -- | Validity of transaction.
  Bool context
validateTransaction tx utxos txOwners = fst $ validateTransactionWithAssetDiff tx utxos txOwners

{- | Validate a 'Transaction'.

To check:
  * Transaction input belongs to valid UTxO set.
  * There is at least one input with owner's address.
  * All outputs contain non-negative value.
  * Circuit corresponding to owner's address outputs 0.
-}
validateTransactionWithAssetDiff ::
  forall context.
  Signature context =>
  -- | 'Transaction' to validate.
  Transaction context ->
  -- | UTxO set.
  UTxO context ->
  -- | List of owners of inputs.
  List context (Address context) ->
  -- | Validity of transaction along with value difference between outputs and inputs.
  (Bool context, AssetValues context)
validateTransactionWithAssetDiff tx utxos txOwners =
  let
    -- Is transaction valid?
    resTxAccValidity :: Bool context = undefined
    ( -- Generated value.
      resTxAccOutValues :: AssetValues context
      , _ :: Address context
      , -- Does output contain non-negative value?
        resTxAccIsOutValid :: Bool context
      ) =
        Symbolic.List.foldl
          ( Morph \((accOutValues :: AssetValues s, accTxOwner :: Address s, accIsOutValid :: Bool s), txOut :: Output s) ->
              ( ifThenElse
                  (txoAddress txOut == accTxOwner)
                  accOutValues
                  (addAssetValues (txoValue txOut) accOutValues)
              , accTxOwner
              , accIsOutValid && (assetValuesNonNegative (txoValue txOut))
              )
          )
          (emptyAssetValues :: AssetValues context, txOwner tx, true :: Bool context)
          (txOutputs tx)
    ( -- Consumed value.
      resTxAccInValues :: AssetValues context
      , _ :: Address context
      , -- Does transaction have at least one spent input?
        resTxAccAtleastOneSpentInput :: Bool context
      ) =
        Symbolic.List.foldl
          ( Morph \((accInValues :: AssetValues s, accTxOwner :: Address s, accAtleastOneSpentInput :: Bool s), txInput :: Input s) ->
              let out = txiOutput txInput
               in ( ifThenElse
                      (txoAddress out == accTxOwner)
                      (accInValues)
                      (addAssetValues (txoValue $ txiOutput txInput) accInValues)
                  , accTxOwner
                  , accAtleastOneSpentInput || (txoAddress out == accTxOwner)
                  )
          )
          (emptyAssetValues :: AssetValues context, txOwner tx, false :: Bool context)
          (txInputs tx)
   in
    ( resTxAccValidity
        && resTxAccAtleastOneSpentInput
        && resTxAccIsOutValid
    , addAssetValues resTxAccOutValues (negateAssetValues resTxAccInValues)
    )
